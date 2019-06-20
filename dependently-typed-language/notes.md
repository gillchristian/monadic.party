Simple lamba (takes term, returns temr)

```
\x -> x
```

First generalization, we include types:

```
forall A : a \x: a -> x
```

Then we have functions on type level (e.g. list of type A) and then dependent
types.

E.g. vectors dependent on naturals (size) and a type (values of the vector)

:point_up: becomes much more expressive.

**Evaluation rules**

(in the notebook)

**Context & well-formed types**

How to say that `n` or `x` are of type `a`

**Type rules** (inference & checking)

`↑` -> type inference: try to figure out the type of something

`↓` -> type check: get the type and check that is valid

Bidirectional type checking (needed for dependent types)

To read: [De Bruijn index](https://en.wikipedia.org/wiki/De Bruijn index)

We give numbers (indexes) to the variable name so when they have the same name
(shadowing) we don't bind the wrong ones.

```
(\x -> x) 5 -- x gets replaced with 5
```

```
(\x -> (\x -> x)) 5 -- we can't replace all the x's with 5, they are different
(\5 -> 5)           -- we'd get the wrong result
```

To that example we can assign indexes (outer is 0, inner is 1)

```
(\0 (\1 -> 1)) 5 -- 5 is the first so shold replace the 0
(\1 -> 1)        -- 0 doesn't appear on the result of application
(\x -> x)        -- we can then transform it back to x
```

Investigate for "unbound" library

---

**Dependent types**

We add syntax for "dependent function space"

```
forall x :: p.p'
```

_NOTE: Here `.` is like the arrow, it's a dependent lambda_

Terms and types now live in the same level -> only one an AST

:point_up: That makes us now need to also check that types are types and values
are values

On **evaluation rules** we add `* || *` (`||` would be the vertical `=>`)

Types are part of the function:

```
Const :: x:* -> y:* -> a:x -> b:y -> a
```

We use/define equallity (of types and values) as:

> "when two things reduce to the same normal form they are the same thing"

E.g. `p` and `t` are equal here:

```
G |- (e :: p) ::↑ t
```

"intentional equallity" -> compare the types (i.e. the types are the intention)

```
(+) :: Num a => a -> a

-- equals

(-) :: Num a => a -> a
```

`Int -> Int` is equivalent `forall x :: a:x -> x` applied to `Int`

"extensional equallity" -> compare the results

> "totally different functions that produce the same results"

To read:
[True Concurrency of Deep Inference Proofs](https://drive.google.com/file/d/0B150TbHH-gFtWVE0dGJMOVFEeTQ/view)

---

Playing around with our type system:

```
LP> let id = (\a x -> x) :: forall (a :: *) (x :: a) . x -> x
LP> let id = (\a x -> x) :: forall (a :: *) . a -> a
id :: forall (x :: *) (y :: x) . x
LP> :t id
forall (x :: *) (y :: x) . x
LP> :t id id
type mismatch:
type inferred:  forall (x :: *) (y :: x) . x
type expected:  *
for expression: id
LP> :t id (forall (a :: *) . a -> a)
forall (x :: forall (x :: *) (y :: x) . x) (y :: *) (z :: y) . y
LP> :t id (forall (a :: *) . a -> a) id
forall (x :: *) (y :: x) . x
```

```
LP> let const = (\ ta tb a b -> a ) :: forall (ta :: *) (tb :: *) (a :: ta) (b :: tb) . ta -> tb -> ta
const :: forall (x :: *) (y :: *) (z :: x) (a :: y) . x
```
