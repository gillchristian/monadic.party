{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cli
  ( main
  ) where

import qualified Data.ByteString                      as BS
import qualified Database.PostgreSQL.Simple           as Pg
import qualified Database.PostgreSQL.Simple.FromField as PgFromField
import qualified Database.PostgreSQL.Simple.ToField   as PgToField
import qualified Database.PostgreSQL.Simple.Types     as PgTypes
import           GHC.Generics                         (Generic)

data EventType
  = Upgrade
  | Downgrade
  deriving (Show, Eq)

-- https://github.com/lpsmith/postgresql-simple/issues/110#issuecomment-497675807
instance PgFromField.FromField EventType where
  fromField f (Just "upgrade") = pure Upgrade
  fromField f (Just "downgrade") = pure Downgrade
  fromField f (Just bs) =
    PgFromField.returnError Pg.ConversionFailed f (show bs)
  fromField f Nothing = PgFromField.returnError Pg.UnexpectedNull f ""

instance PgToField.ToField EventType where
  toField Upgrade   = PgToField.Escape "upgrade"
  toField Downgrade = PgToField.Escape "downgrade"

data Revision = Revision
  { rId  :: Int
  , rRev :: BS.ByteString
  } deriving (Show, Eq, Generic, Pg.ToRow, Pg.FromRow)

data Event = Event
  { eId      :: Int
  , eRev     :: BS.ByteString
  , eApplied :: BS.ByteString -- TODO: Date?
  , eType    :: EventType
  } deriving (Show, Eq, Generic, Pg.ToRow, Pg.FromRow)

executeSqlFile :: Pg.Connection -> FilePath -> IO ()
executeSqlFile con path = do
  content <- PgTypes.Query <$> BS.readFile path
  res <- Pg.execute_ con content
  putStrLn $ "Exit code: " <> show res

markActiveRevision :: Pg.Connection -> Revision -> IO ()
markActiveRevision con rev = do
  Pg.execute
    con
    "update schemactl_rev set rev = ? where id = ?;"
    (rRev rev, rId rev)
  pure ()

main :: IO ()
main = do
  putStrLn "Hey Monadic Party!"
  let info =
        Pg.defaultConnectInfo
          { Pg.connectUser = "boring-haskell-test"
          , Pg.connectPassword = "test"
          , Pg.connectDatabase = "boring-haskell-test"
          }
  con <- Pg.connect info
  executeSqlFile con "./db/000_bootstrap.sql.up"
  (res :: [Revision]) <- Pg.query_ con "select * from schemactl_rev"
  print $ head res
  let rev2 = Revision 1 "001"
  markActiveRevision con rev2
