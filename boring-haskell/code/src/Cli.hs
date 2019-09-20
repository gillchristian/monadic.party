{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cli where

import           Control.Monad                        (forM_, mapM_, void, when)
import qualified Data.Attoparsec.ByteString.Char8     as P
import qualified Data.ByteString.Char8                as Bs
import qualified Data.List                            as L
import qualified Data.List                            as List
import qualified Data.Maybe                           as M
import           Data.String                          (IsString (..))
import qualified Data.Time                            as Time
import qualified Database.PostgreSQL.Simple           as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow   as Pg
import qualified Database.PostgreSQL.Simple.ToField   as Pg
import qualified Database.PostgreSQL.Simple.ToRow     as Pg
import qualified Database.PostgreSQL.Simple.Types     as Pg
import           GHC.Generics                         (Generic)
import qualified System.Directory                     as Dir
import qualified System.Environment                   as Env
import qualified System.Exit                          as Exit

-- TODO
-- [ ] error reporting
-- [ ] verbose mode
-- [ ] db info from flags (cli lib?)
-- [ ] safer types (data Migrations = Migrations Init [Migration])
-- [ ] specify from/until (if current is higher/lower: fail? do nothing?)
--       $ schemaclt up 005   # from
--       $ schemaclt down 005 # until
connectInfo =
  Pg.defaultConnectInfo
    { Pg.connectUser = "boring-haskell-test"
    , Pg.connectPassword = "test"
    , Pg.connectDatabase = "boring-haskell-test"
    }

main :: IO ()
main = Env.getArgs >>= main'

main' :: [String] -> IO ()
main' args = do
  event <- dieOnLeft $ parseArgs args
  -- TODO: provide usage info ^^^^^^^
  conn <- Pg.connect connectInfo
  -- bootstrap migration is idempotent
  executeSqlFile conn "db/000_bootstrap.sql.up"
  activeRev <- getActiveRev conn
  indexFileContent <- Bs.readFile "db/schemactl-index"
  allMigrations <- dieOnLeft $ P.parseOnly (indexFileP "db/") indexFileContent
  toRun <- dieOnLeft $ getMigrationsToRun event activeRev allMigrations
  runAll conn event toRun

indexFileP :: FilePath -> P.Parser [Migration]
indexFileP base = setParents <$> P.many1 (migrationP base Nothing)

dieOnLeft :: Either String a -> IO a
dieOnLeft = either Exit.die pure

setParents :: [Migration] -> [Migration]
setParents ms = snd $ L.mapAccumL setParent Nothing ms

setParents' :: [Migration] -> Either String Migrations
setParents' ms = do
  init <- maybe (Left "bootstrap migration missing") pure (head' ms)
  let rest = tail' ms
  let ms' = snd $ L.mapAccumL setParent' (mRev init) rest
  let ms'' = zipWith foo ms' rest
  pure $ Migrations (migrationToInit init) ms''

migrationToInit :: Migration -> Init
migrationToInit m = Init r d f
  where
    r = mRev m
    d = mDescription m
    f = mBaseFile m

foo :: Rev -> Migration -> Migration'
foo rev m = Migration' r d rev f
  where
    r = mRev m
    d = mDescription m
    f = mBaseFile m

setParent' :: Rev -> Migration -> (Rev, Rev)
setParent' parent m = (mRev m, parent)

setParent :: Maybe Rev -> Migration -> (Maybe Rev, Migration)
setParent parent m = (Just $ mRev m, m {mParent = parent})

runAll :: Pg.Connection -> EventType -> [Migration] -> IO ()
runAll conn Upgrade ms   = forM_ ms $ runMigration conn Upgrade
runAll conn Downgrade ms = forM_ (reverse ms) $ runMigration conn Downgrade

migrationP :: FilePath -> Maybe Rev -> P.Parser Migration
migrationP base parent
  -- One Migration per line, format:
  -- "000_bootstrap.sql\n"
  -- "001_migration_name.sql\n"
 = do
  rev <- P.takeWhile (/= '_')
  P.char '_'
  desc <- P.takeWhile (/= '.')
  P.string ".sql"
  P.endOfLine
  pure
    Migration
      { mRev = Rev rev
      , mDescription = desc
      , mParent = parent
      , mBaseFile = base <> Bs.unpack (rev <> "_" <> desc <> ".sql")
      }

parseArgs :: [String] -> Either String EventType
parseArgs []         = pure Upgrade
parseArgs ("up":_)   = pure Upgrade
parseArgs ("down":_) = pure Downgrade
parseArgs _          = Left "Wrong args"

getMigrationsToRun ::
     EventType -> Rev -> [Migration] -> Either String [Migration]
getMigrationsToRun event activeRev ms = do
  let toRun = filterToRun event activeRev ms
  -- TODO: add validation here
  when False (Left "wrong migration")
  pure toRun

filterToRun :: EventType -> Rev -> [Migration] -> [Migration]
filterToRun Upgrade activeRev = filter $ \m -> unRev (mRev m) > unRev activeRev
filterToRun Downgrade activeRev =
  filter $ \m -> unRev (mRev m) <= unRev activeRev

executeSqlFile :: Pg.Connection -> FilePath -> IO ()
executeSqlFile conn filePath
  -- Only do this for trusted inputs.
  -- This goes around the type safe API which prevents SQL injections.
 = void $ Pg.execute_ conn =<< Pg.Query <$> Bs.readFile filePath

runMigration ::
     Pg.Connection -> EventType -> Migration -> IO (Either String NewEvent)
runMigration conn Upgrade migration =
  runMigration' conn Upgrade (mRev migration) migration
runMigration conn Downgrade migration =
  let rev = M.fromMaybe (mRev migration) $ mParent migration
   in runMigration' conn Downgrade rev migration

runMigration' ::
     Pg.Connection
  -> EventType
  -> Rev
  -> Migration
  -> IO (Either String NewEvent)
runMigration' conn eType rev migration = do
  migrationFile <- mFile eType migration
  let newEvent = fromMigration eType migration
  case migrationFile of
    Just file ->
      Pg.withTransaction conn $ do
        executeSqlFile conn file
        insertNewEvent conn newEvent
        markActiveRevision conn rev
        pure $ pure newEvent
    Nothing -> pure $ Left "no file to run"

-- TODO:
-- data Migrations = Migrations Init [Migration]
-- Init        -> Bootstrap migration, doesn't have parents
-- [Migration] -> The rest of the migrations, they have parents
data Migrations =
  Migrations Init
             [Migration']
  deriving (Eq, Show)

data Init = Init
  { iRev         :: Rev
  , iDescription :: Bs.ByteString
  , iBaseFile    :: FilePath
  } deriving (Eq, Show)

data Migration' = Migration'
  { mRev'         :: Rev
  , mDescription' :: Bs.ByteString
  , mParent'      :: Rev
  , mBaseFile'    :: FilePath
  } deriving (Eq, Show)

data Migration = Migration
  { mRev         :: Rev
  , mDescription :: Bs.ByteString
  , mParent      :: Maybe Rev
  , mBaseFile    :: FilePath
  } deriving (Eq, Show)

instance Pg.ToRow Rev where
  toRow rev = [Pg.toField rev]

newtype Rev = Rev
  { unRev :: Bs.ByteString
  } deriving (Eq, Show)

instance IsString Rev where
  fromString = Rev . Bs.pack

instance Pg.ToField Rev where
  toField = Pg.Escape . unRev

instance Pg.FromField Rev where
  fromField f dat = Rev <$> Pg.fromField f dat

instance Pg.FromRow Rev where
  fromRow = Pg.field

mFile :: EventType -> Migration -> IO (Maybe FilePath)
mFile Upgrade m   = requireFile $ mBaseFile m ++ ".up"
mFile Downgrade m = requireFile $ mBaseFile m ++ ".down"

requireFile :: FilePath -> IO (Maybe FilePath)
requireFile filePath = do
  res <- Dir.doesFileExist filePath
  if res
    then pure $ Just filePath
    else pure Nothing

data EventType
  = Upgrade
  | Downgrade
  deriving (Show, Eq)

instance Pg.ToField EventType where
  toField Upgrade   = Pg.Escape "upgrade"
  toField Downgrade = Pg.Escape "downgrade"

data NewEvent = NewEvent
  { eMigrationRev :: Rev
  , eType         :: EventType
  } deriving (Show, Eq, Generic, Pg.ToRow)

fromMigration :: EventType -> Migration -> NewEvent
fromMigration eType migration =
  NewEvent {eMigrationRev = mRev migration, eType = eType}

insertnewEventQuery :: Pg.Query
insertnewEventQuery =
  "insert into schemactl_events (rev, event_type) values (?, ?)"

insertNewEvent :: Pg.Connection -> NewEvent -> IO ()
insertNewEvent conn event = void $ Pg.execute conn insertnewEventQuery event

markActiveRevision :: Pg.Connection -> Rev -> IO ()
markActiveRevision conn rev =
  void $ Pg.execute conn "update schemactl_rev set rev = ?" rev

getActiveRev :: Pg.Connection -> IO Rev
getActiveRev conn = do
  res <- Pg.query_ conn "select rev from schemactl_rev limit 1"
  pure (head res) -- Safe, because of our Bootstrap migration

tail' :: [a] -> [a]
tail' []     = []
tail' (_:xs) = xs

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = pure x
