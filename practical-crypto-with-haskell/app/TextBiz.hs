{-# LANGUAGE OverloadedStrings #-}

module TextBiz where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

helloWorld :: T.Text
helloWorld = "hello world"

printHelloWorld :: IO ()
printHelloWorld = TIO.putStrLn helloWorld

joined = helloWorld <> "\n" <> helloWorld

data Mode
  = Encrypt
  | Decrypt
  deriving (Eq, Show)

encode mode str =
  if mode == Encrypt
    then "encrypt"
    else "decrypt"

-- pattern match on data
encode' Encrypt str = "encrypt"
encode' Decrypt str = "decrypt"

-- patern match on values
isEmpty "" = True
isEmpty _  = False

-- build reverse using: T.length  T.take  T.snoc T.last
reverse' :: T.Text -> T.Text
reverse' s = go s ""
  where
    go "" to = to
    go from to = go rest $ T.snoc to $ T.last from
      where
        l = T.length from - 1
        rest = T.take l from

reverser :: T.Text -> T.Text
reverser = T.foldr (flip T.snoc) ""

reversel :: T.Text -> T.Text
reversel = T.foldl (flip T.cons) ""
