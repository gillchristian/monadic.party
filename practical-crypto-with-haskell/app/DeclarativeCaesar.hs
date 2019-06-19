{-# LANGUAGE OverloadedStrings #-}

module DeclarativeCaesar where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

encryptChar :: Char -> Char
encryptChar 'z' = 'a'
encryptChar 'Z' = 'A'
encryptChar c   = succ c

decryptChar :: Char -> Char
decryptChar 'a' = 'z'
decryptChar 'A' = 'Z'
decryptChar c   = pred c

transpositionCipher key = T.concat . T.transpose . T.chunksOf key

-- TODO: finish \o/
