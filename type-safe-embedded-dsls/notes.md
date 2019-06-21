We write code to solve problems in different **domains**

We express the solutions to those problems in the language

```
DSL -> domain + language
DSL -> domain + syntax + semantics
```

In Haskell we can model languages as:

```hs
data Syntax = ... -- AST

semantics :: Syntax -> _
```

Another definition

```
DSL -> primitives + composition + interpretation
               (syntax)            (semantics) 
```

```hs
data Primitives = ... -- AST

combinator :: Primitives -> Primitives

interpreter :: Primitives -> _
```

:point_down: :point_down: :point_down: Syntax is important, for humans

**Correctness by construction**

Language: a set of things and means of combining them, that's a language

DSLs evolve as you build them