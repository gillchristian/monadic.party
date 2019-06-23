{-# LANGUAGE OverloadedStrings #-}

import           Cli                              hiding (main)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Test.Hspec

first =
  Migration
    { mRev = "000"
    , mDescription = "bootstrap"
    , mBaseFile = "db/000_bootstrap.sql"
    , mParent = Nothing
    }

second =
  Migration
    { mRev = "001"
    , mDescription = "add_users"
    , mBaseFile = "db/001_add_users.sql"
    , mParent = pure $ Rev "000"
    }

third =
  Migration
    { mRev = "002"
    , mDescription = "add_sessions"
    , mBaseFile = "db/002_add_sessions.sql"
    , mParent = pure $ Rev "001"
    }

allMigrations = [first, second, third]

indexFileContent =
  "000_bootstrap.sql\n" <>
  "001_add_users.sql\n" <>
  "002_add_sessions.sql\n"

main :: IO ()
main =
  hspec $ do
    describe "filterToRun" $ do
      it "filters up migrations" $ do
        length (filterToRun Upgrade (Rev "000") allMigrations) `shouldBe` (2 :: Int)
        length (filterToRun Upgrade (Rev "001") allMigrations) `shouldBe` (1 :: Int)
        length (filterToRun Upgrade (Rev "002") allMigrations) `shouldBe` (0 :: Int)
        length (filterToRun Upgrade (Rev "003") allMigrations) `shouldBe` (0 :: Int)
      it "filters down migrations" $ do
        length (filterToRun Downgrade (Rev "000") allMigrations) `shouldBe` (1 :: Int)
        length (filterToRun Downgrade (Rev "001") allMigrations) `shouldBe` (2 :: Int)
        length (filterToRun Downgrade (Rev "002") allMigrations) `shouldBe` (3 :: Int)
        length (filterToRun Downgrade (Rev "003") allMigrations) `shouldBe` (3 :: Int)
    describe "migrationP" $
      it "parses a migration" $ do
        P.parseOnly (migrationP "db/" Nothing) "000_bootstrap.sql\n" `shouldBe` pure first
        P.parseOnly (migrationP "db/" $ pure $ Rev "000") "001_add_users.sql\n" `shouldBe` pure second
        P.parseOnly (migrationP "db/" $ pure $ Rev "001") "002_add_sessions.sql\n" `shouldBe` pure third
    describe "indexFileP" $
      it "parses an index file" $
        P.parseOnly (indexFileP "db/") indexFileContent `shouldBe` pure allMigrations
    describe "parseArgs" $ do
      it "defaults to `Upgrade`" $
        parseArgs [] `shouldBe` pure Upgrade
      it "parses up/down" $ do
        parseArgs ["up"] `shouldBe` pure Upgrade
        parseArgs ["down"] `shouldBe` pure Downgrade
      it "errors on other arguments" $ do
        parseArgs ["foo"] `shouldBe` Left "Wrong args"
        parseArgs ["foo", "up"] `shouldBe` Left "Wrong args"
