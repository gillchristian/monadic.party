{-# LANGUAGE OverloadedStrings #-}

import           Cli         hiding (main)
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    let allMigrations =
          [ Migration
              { mRev = "000"
              , mDescription = "bootstrap"
              , mBaseFile = "db/000_bootstrap.sql"
              }
          , Migration
              { mRev = "001"
              , mDescription = "add_users"
              , mBaseFile = "db/001_add_users.sql"
              }
          , Migration
              { mRev = "002"
              , mDescription = "add_sessions"
              , mBaseFile = "db/002_add_sessions.sql"
              }
          ]
    describe "getMigrationsToRun" $ do
      it "filters up migrations" $ do
        length (getMigrationsToRun Upgrade Rev {unRev = "000"} allMigrations) `shouldBe` (3 :: Int)
        length (getMigrationsToRun Upgrade Rev {unRev = "001"} allMigrations) `shouldBe` (2 :: Int)
        length (getMigrationsToRun Upgrade Rev {unRev = "002"} allMigrations) `shouldBe` (1 :: Int)
        length (getMigrationsToRun Upgrade Rev {unRev = "003"} allMigrations) `shouldBe` (0 :: Int)
      it "filters down migrations" $ do
        length (getMigrationsToRun Downgrade Rev {unRev = "000"} allMigrations) `shouldBe` (1 :: Int)
        length (getMigrationsToRun Downgrade Rev {unRev = "001"} allMigrations) `shouldBe` (2 :: Int)
        length (getMigrationsToRun Downgrade Rev {unRev = "002"} allMigrations) `shouldBe` (3 :: Int)
        length (getMigrationsToRun Downgrade Rev {unRev = "003"} allMigrations) `shouldBe` (3 :: Int)
