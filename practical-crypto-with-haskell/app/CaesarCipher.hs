{-# LANGUAGE OverloadedStrings #-}

module CaesarCipher where

import Data.Maybe
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data Mode
  = Encrypt
  | Decrypt
  deriving (Show, Eq)

secretMessage :: T.Text
secretMessage = "This is up to you to add"

symbols :: T.Text
symbols = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

-- encryptChar Encrypt 5 'a' -- 'f'
-- TODO: wrap around
-- encryptChar Encrypt 5 'z' -- 'E'
encryptChar :: Mode -> Int -> Char -> Char
encryptChar Encrypt key c = shiftChar key c
encryptChar Decrypt key c = shiftChar (-key) c

shiftChar :: Int -> Char -> Char
shiftChar key c = T.index symbols (fromMaybe key index)
  where
    index = (+ key) <$> T.findIndex (== c) symbols

-- encryptMessage mode key
--   = T.foldl (\result c -> T.snoc result (encryptChar mode key c)) ""
encryptMessage mode key = T.map (encryptChar mode key)
