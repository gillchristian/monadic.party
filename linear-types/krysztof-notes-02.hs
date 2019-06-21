{-# LANGUAGE LinearTypes, GADTs, UnicodeSyntax #-}
module Arr where

import Prelude hiding (read, foldl)

data Unrestricted a where
   Unrestricted :: a -> Unrestricted a

foldl :: (b ⊸  a -> b) -> b ⊸  [a] -> b
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs



data MArray a
data Array a

newMArray :: Int → (MArray a ⊸ Unrestricted b) ⊸ b
newMArray = undefined

write :: MArray a⊸ (Int,a) → MArray a
write = undefined

read :: MArray a ⊸ Int → (MArray a, Unrestricted a)
read = undefined

freeze :: MArray a ⊸ Unrestricted (Array a)
freeze = undefined

{-
fibs n = newMArray n (\arr -> freeze (foldl g (init arr) [2..n-1]))
   where init :: MArray Int ->. MArray Int
         init arr = foldl write arr [(0, 0), (1, 1)]

         g :: MArray Int ->. Int -> MArray Int
         g arr i = let (arr',  Unrestricted fib1) = read arr (i-1)
                       (arr'', Unrestricted fib2) = read arr' (i-2)
                   in write arr'' (i, fib1 + fib2)

-}


fibs :: Int -> Array Int
fibs n = newMArray n (\arr -> freeze (foldl g (init arr) [2..n-1]))
   where g :: MArray Int ->. Int -> MArray Int
         g arr i = h i (read arr (i-1))
         init :: MArray Int ->. MArray Int
         init arr = foldl write arr [(0, 0), (1, 1)]

         h :: Int -> (MArray Int, Unrestricted Int) ->. MArray Int
         h i (arr, Unrestricted fib1) = h' i (read arr (i-2)) fib1
      
         h' :: Int -> (MArray Int, Unrestricted Int) ->. Int -> MArray Int
         h' i (arr, Unrestricted fib2) fib1 = write arr (i, fib1 + fib2)
