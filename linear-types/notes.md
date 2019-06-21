Linear types, currently at work in tweag

Linear arguments should be used once and only once

```hs
{-# LANGUAGE LinearTypes #-}

module L where

identity :: a ->. a
identity a = a

f :: a ->. a
f a = (a, a) -- fails 
```

In Haskell values are information, they can be ignored or duplicated

With liniarity, values become resources (?)

```hs
{-# LANGUAGE LinearTypes #-}

module L where

apply :: (a ->. b) -> a -> b
apply f x = f x
apply f = f -- doesn't work, linear functions aren't casted to regular functions
```

```hs
{-# LANGUAGE LinearTypes #-}

module L where

swap :: (a, b) ->. (b, a)
swap (a, b) = (b, a)
```

Liniarity must be preserved, there are several more rules.

Linear types are a static check, all the rules would actually work in rune time

E.g. a linear function can be called twice in rune time but with tell the
compiler to prevent us from doing so

:point_up: this adds restrictions, just like type classes add restrictions

Less powerful with more guarantees

Goal is to make some linear functions -> use them in libraries to add more guarantees

```hs
data T a = MkT a -- MkT is linear by default

data T a where
  MkT :: a ->. T a -- MkT is linear

data T a where
  MkT :: a -> T a -- MkT is NOT linear

data Pair a b where
  Pair :: a ->. -> b -> Pair a b -- only `a` is linear
```

```hs
data Unrestricted a where
  Unrestricted :: a -> Unrestricte a

convert (a -> b) ->. Unrestricted a ->. b
convert f (Unrestricted x) = f x

convert' (Unrestricted a ->. b) ->. a ->. b
convert' f x = f (Unrestricted x)
```

Monads   -> easy to get in, hard to get out

Comonads -> easy to get out, hard to get in

`Unrestricted` is Comonad
Easy to have something linear and make it unrestricted
Hard to make it linear when is not

Product & coproduct

```hs
{-
a, b

The product is a tyep T a b
such that we have projections
  T a b -> a
  T a b -> b

The coproduct is a tyep T a b
such that we have injections
  a -> T a b
  b -> T a b
-}

-- product doesn't work (doesn't use `b`)
fst :: (a, b) ->. a
fst (a, _) = a

-- coproduct works (also for `right`)
left :: a ->. Either a b
left = Left

-- product
vend :: Coin ->. Either Tea Coffe -- get only one, works

-- coproduct
vend :: Coin ->. (Tea, Coffe)     -- get both, doesn't works
```

CoX -> invert the arrows (works for everything)

E.g.

```hs
-- how to construct
data X where
  M :: Int -> X
  N :: Bool -> X

-- how to deconstruct (pattern match?)
codata X where
  M :: X -> Int
  N :: X -> Bool

-- somehow this works
codata Prod a b where
  Fst :: Prod a b ->. a
  Snd :: Prod a b ->. b
```

```hs
-- works, we create a new one
f :: Bool ->. Unrestricted Bool
f False = Unrestricted False
f True = Unrestricted True

-- doesn't work, x is consumed inlinearly
f :: x ->. Unrestricted x
f x = Unrestricted x
```

**Paper**: https://www.tweag.io/posts/2017-08-24-linear-types-packed-data.html
