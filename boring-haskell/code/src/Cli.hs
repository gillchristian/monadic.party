{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cli
  ( main
  ) where

import qualified Data.ByteString                  as BS
import qualified Database.PostgreSQL.Simple       as Pg
import qualified Database.PostgreSQL.Simple.Types as PgTypes

executeSqlFile :: Pg.Connection -> FilePath -> IO ()
executeSqlFile con path = do
  content <- PgTypes.Query <$> BS.readFile path
  res <- Pg.execute_ con content
  putStrLn $ "Exit code: " <> show res

{-
-- execute query
let query = "CREATE TABLE IF NOT EXISTS foo (id bigserial primary key);"
res <- Pg.execute_ con query
putStrLn $ "Exit code: " <> show res
-}
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
