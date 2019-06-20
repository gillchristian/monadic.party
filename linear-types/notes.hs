{-
Krzysztof notes

docker pull tweag/linear-types
docker run -it tweag/linear-types:latest
-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module L where

import Prelude hiding (map)
import Unsafe.Coerce

identity :: a ⊸  a
identity x = x

f :: a ->. b -> a
f !x y = x

seq (!x, y) = (x, y)

apply :: (a -->.(m) b) -> a -->.(m) b
apply f x = f x

flip :: (a ->. b ->. c) ->. b ->. a ->. c
flip f x y = f y x

ifte :: Bool ->. a -> a -> a
ifte True x y = x
ifte False x y = y

add :: [a] ->. [a] ->. [a]
add [] x = x

{-
To consume an argument linearly you can:
- return it unmodified
- pass it to a linear function
- if the argument is a function, you can call it and then consume the result of this call linearly
- if the argument is a data type, you can pattern match on it and consume all fields linearly
-}

{-

partition :: [a ->. (Bool, a)] -> [a] ->. ([a], [a])
partition f [] = ([], [])
partition f (x:xs) = h (f x) (partition f xs)

h :: (Bool, a) ->. ([a], [a]) ->. ([a], [a])
h (True, x')  (ys, zs) = (x' : ys, zs)
h (False, x') (ys, zs) = (ys, x' : zs)
-}


mapL :: (a ->. b) -> [a] ->. [b]
mapL f [] = []
mapL f (x:xs) = f x : mapL f xs


data Pair a b where
  Pair :: a ->. b -> Pair a b

uncur :: (a ->. b -> c) ->. Pair a b ->. c
uncur f (Pair x y) = f x y

data Unrestricted a where
  Unrestricted :: a -> Unrestricted a

convert :: (a -> b) ->. Unrestricted a ->. b
convert f (Unrestricted x) = f x

convert2 :: (Unrestricted a ->. b) ->. a -> b
convert2 f x = f (Unrestricted x)

{-
comp :: IO b -> (b -> IO c) -> IO c
join :: IO (IO c) -> IO c

comp :: (Unrestricted a ->. b) ->.
        Unrestricted a ->. Unrestricted b
-}

extract :: Unrestricted a ->. a
extract (Unrestricted x) = x

duplicate :: Unrestricted a ->. Unrestricted (Unrestricted a)
duplicate (Unrestricted x) = Unrestricted (Unrestricted x)

{-
a, b
The product is a type T a b
such that we have projections T a b -> a
and T a b -> b

The coproduct is a type T' a b
such that we have injections a -> T' a b
and b -> T' a b
-}

{-
left :: a ->. Either a b
left = Left

fst' :: (a,b) ->. a
fst' (x,_) = x

fst'' :: Prod a b ->. a
fst'' (x,_) = x

diag :: a ->. Prod a a
diag x = (x,x)

data X where
  F :: Int -> Bool -> X
  G :: X


codata Prod a b where
  Fst :: Prod a b ->. a
  Snd :: Prod a b ->. b

type Prod a b = forall r. Either (a ->. r)
                                 (b ->. r) ->. r
-}

-- Unrestricted type can be thought of
-- "information" that can be duplicated and
-- discarded at will

class Movable a where
  move :: a ->. Unrestricted a

fmap' :: (a ->. b) -> Unrestricted a ->. Unrestricted b
fmap' f (Unrestricted x) = Unrestricted (f x)

pair :: Unrestricted a ->. Unrestricted b ->.        Unrestricted (a,b)
pair (Unrestricted x) (Unrestricted y) = Unrestricted (x,y)

instance Movable Bool where
  move False = Unrestricted False
  move True = Unrestricted True

instance (Movable a, Movable b) => Movable (Either a b) where
  move (Left x) = fmap' Left (move x)
  move (Right x) = fmap' Right (move x)

instance (Movable a, Movable b) => Movable (a,b) where
  move (x, y) = pair (move x) (move y)

instance Movable (Unrestricted a) where
  move = duplicate

instance Movable a => Movable [a] where
  move x = tr (mapL move x)
           -- [Unrestricted a]
           -- Unrestricted [a]

tr :: [Unrestricted a] ->. Unrestricted [a]
tr [] = Unrestricted []
tr (x:xs) = liftA2L (:) x (tr xs)

liftA2L :: (a ->. b ->. c) -> Unrestricted a ->. Unrestricted b ->. Unrestricted c
liftA2L f (Unrestricted x) (Unrestricted y) = Unrestricted (f x y)

-- Movable (Array a)


data Mult m a where
  MkMult :: a -->.(m) Mult m a

-- a ->.(m) b ~ Mult m a       ->. b
-- (a -> b)   ~ Unrestricted a ->. b


{-
(&&&) :: (a -->.(m) b) ->. (a -->.(n) c) ->.
     a -->.(m + n) (b,c)
(.) :: (b ->{m} c) -> (a ->{n} b) -> a ->{m * n} c
eihter :: (a -->.(m) c) -> (b -->.(n) c)  -> Either a b -->.(sup m n) c

Mult m (Mult n a)   ~ Mult (m * n) a
Mult (m + n) a      ~ Mult m a, Mult n a
Either (Mult m a) (Mult n a) -> Mult (sup m n) a


Affine = {0,1}
Omega = {0,1,2,...}
One   = {1}
-}

z :: [a] -> [b] ->. [(a,b)]
z [] [] = []
z (x:xs) [] = []
z (x:xs) (y:ys) = (x,y) : z xs ys

{-
p :: (a -> Bool) -> [a] -> ([a], [a])

s :: (a -> a -> Ordering) -> [a] -> [a]
-}

convert' :: (a -> b) ->. a ->. b
convert' = unsafeCoerce id

{-
f :: Void -> a ->. a
f x y = case x of {}
-}

{-
How pointfree works.

[\x -> y] = const y
[\x -> x] = id
[\x -> f y] = ap [\x -> f] [\x -> y]
-- K=const, S=ap, I=id
-- SKI combinators

[\x -> y] impossible
[\x -> x] = id
[\x -> flip y (... x ...)]
[\x -> f (... x...)] = f . [\x -> ... x ...]
-- B=composition, C=flip, I=id
--
-}
{-
join f x = f x x

f (x,y) = (y,x)

[a] ->. [a] ->. [a]


\x -> sin (...)   => sin . \x ...
-}

{-
func :: (Int -> a) -> [a]
func f = map f [10, 20, 40]
-- Yoneda lemma

func' :: (Int -> a) -> a ->. [a]
func' f x = [f 5, f 10, x, f 30, f 100, f 6]
-- Zipper
-}

-- Zipper of (a,a,a)
-- data TripleZipper a = Left (a,a) |
--                       Middle (a,a) | 
--                       Bottom (a,a)
--
-- (a^3)' = 3a^2
--
-- (f + g)' = f' + g'
-- (f * g)' = f' * g + f * g'

data Equal a b where
  MkEq :: Equal a a

c :: Equal a b -> a -> b
c MkEq x = x
