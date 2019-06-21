{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cli where

import           Control.Monad                        (forM_, when)
import qualified Data.Attoparsec.ByteString.Char8     as P
import qualified Data.ByteString.Char8                as Bs
import qualified Data.List                            as L
import qualified Data.List                            as List
import           Data.String                          (IsString (..))
import qualified Data.Time                            as Time
import qualified Database.PostgreSQL.Simple           as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow   as Pg
import qualified Database.PostgreSQL.Simple.ToField   as Pg
import qualified Database.PostgreSQL.Simple.ToRow     as Pg
import qualified Database.PostgreSQL.Simple.Types     as PgTypes
import           GHC.Generics                         (Generic)
import qualified System.Directory                     as Dir
import qualified System.Environment                   as Env
import qualified System.Exit                          as Exit

main :: IO ()
main = do
  args <- Env.getArgs
  event <- either Exit.die pure $ parseArgs args
  let connectInfo =
        Pg.defaultConnectInfo
          { Pg.connectUser = "boring-haskell-test"
          , Pg.connectPassword = "test"
          , Pg.connectDatabase = "boring-haskell-test"
          }
  conn <- Pg.connect connectInfo
  executeSqlFile conn "db/000_bootstrap.sql.up"
  activeRev <- getActiveRev conn
  indexFileContent <- Bs.readFile "db/schemactl-index"
  allMigrations <-
    either Exit.die pure $ P.parseOnly (indexFileP "db/") indexFileContent
  toRun <-
    either Exit.die pure $ getMigrationsToRun event activeRev allMigrations
  putStrLn $ "All: " <> show allMigrations
  putStrLn $ "To run: " <> show (unRev . mRev <$> toRun)
  putStrLn $ "Active rev: " <> show activeRev
  putStrLn $ "Event: " <> show event
  forM_ toRun $ runMigration conn event

indexFileP :: FilePath -> P.Parser [Migration]
indexFileP base = do
  migrations <- P.many1 (migrationP base Nothing)
  pure $ setParents migrations

setParents :: [Migration] -> [Migration]
setParents ms = snd $ L.mapAccumL f Nothing ms
  where
    f parent m = (Just $ mRev m, m {mParent = parent})

migrationP :: FilePath -> Maybe Rev -> P.Parser Migration
migrationP base parent
  -- rev <- P.takeWhile (/= '_')
  -- P.char '_'
  -- desc <- P.takeWhile (/= '.')
  -- P.string ".sql"
  -- pure
  --   Migration
  --     { mRev = Rev rev
  --     , mDescription = desc
  --     , mParent = parent
  --     , mBaseFile = base <> Bs.unpack (desc <> ".sql")
  --     }
 =
  Migration <$> (Rev <$> P.takeWhile (/= '_')) <*>
  (P.char '_' *> P.takeWhile (/= '.')) <*>
  (Nothing <$ P.string "") <*>
  (Bs.unpack <$> P.string ".sql" <* P.char '\n')

parseArgs :: [String] -> Either String EventType
parseArgs []         = pure Upgrade
parseArgs ("up":_)   = pure Upgrade
parseArgs ("down":_) = pure Downgrade
parseArgs _          = Left "Wrong args"

getMigrationsToRun ::
     EventType -> Rev -> [Migration] -> Either String [Migration]
getMigrationsToRun event activeRev ms = do
  let toRun = filterMigrationsToRun event activeRev ms
  -- TODO: add validation here
  when False (Left "wrong migration")
  pure toRun

filterMigrationsToRun :: EventType -> Rev -> [Migration] -> [Migration]
filterMigrationsToRun Upgrade activeRev =
  filter $ \m -> unRev (mRev m) > unRev activeRev
filterMigrationsToRun Downgrade activeRev =
  filter $ \m -> unRev (mRev m) <= unRev activeRev

executeSqlFile :: Pg.Connection -> FilePath -> IO ()
executeSqlFile conn filePath
  -- Only do this for trusted inputs. This goes around the type safe
  -- API which prevents SQL injections.
 = do
  query <- PgTypes.Query <$> Bs.readFile filePath
  Pg.execute_ conn query
  pure ()

runMigration :: Pg.Connection -> EventType -> Migration -> IO NewEvent
runMigration conn eType migration = do
  migrationFile <- mFile eType migration
  let newEvent = fromMigration eType migration
  -- TODO:
  -- for downgration we should insert the prev event as the active revision
  case migrationFile of
    Just file ->
      Pg.withTransaction conn $ do
        executeSqlFile conn file
        insertNewEvent conn newEvent
        markActiveRevision conn migration
    Nothing -> pure ()
  pure newEvent

-- TODO:
-- Migrations Init [Migration]
-- Init        -> Bootstrap migration
-- [Migration] -> The rest of the migrations
data Migration = Migration
  { mRev         :: Rev
  , mDescription :: Bs.ByteString
  , mParent      :: Maybe Rev
  , mBaseFile    :: FilePath
  } deriving (Show)

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
mFile Upgrade m   = mUpgradeFile m
mFile Downgrade m = mDowngradeFile m

mUpgradeFile :: Migration -> IO (Maybe FilePath)
mUpgradeFile migration = do
  let upgradeFilePath = mBaseFile migration ++ ".up"
  requireFile upgradeFilePath

mDowngradeFile :: Migration -> IO (Maybe FilePath)
mDowngradeFile migration = do
  let downgradeFilePath = mBaseFile migration ++ ".down"
  requireFile downgradeFilePath

requireFile :: FilePath -> IO (Maybe FilePath)
requireFile filePath = do
  res <- Dir.doesFileExist filePath
  if res
    then pure (pure filePath)
    else Exit.die $ "file " ++ filePath ++ " required but not found"

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

insertNewEvent :: Pg.Connection -> NewEvent -> IO ()
insertNewEvent conn event = do
  Pg.execute
    conn
    "insert into schemactl_events (rev, event_type) values (?, ?)"
    event
  pure ()

markActiveRevision :: Pg.Connection -> Migration -> IO ()
markActiveRevision conn migration = do
  Pg.execute conn "update schemactl_rev set rev = ?" (mRev migration)
  pure ()

getActiveRev :: Pg.Connection -> IO Rev
getActiveRev conn = do
  res <- Pg.query_ conn "select rev from schemactl_rev limit 1"
  pure (head res) -- Safe, because of our Bootstrap migration
