---
marp: true
paginate: true
theme: uncover
class: invert
style: |
  @import url('https://fonts.googleapis.com/css2?family=Fira+Code&family=Fira+Sans:ital@0;1&display=swap');
  section { font-family: "Fira Sans", sans-serif; }
  code { font-family: "Fira Code", monospace; }
---

# **PV Project**

***Status Report***

<br/>
<br/>

<div align="right">
  Alex Ionescu<br/>
  Maksymilian Demitraszek
</div>

---

## **Current Status**

* "Core" implemented
  * Parser & Type-Checker
  * WLP-based verification & Z3 integration
* Correctly works on simple benchmarks
  * Arrays, `forall`, `exists`
* Feasibility checking currently WIP

---

## **Problems We Encountered**

* `z3` setup (& bindings)
* Tedious, repetititive pattern matching
  * Solution: `recursion-schemes` + `Data.Fix`
* Accounting for variable shadowing

---

## **Recursion Schemes**

---

## **Simple AST Representation**

```haskell
data Expr
  = Literal Int
  | Var Name
  | Add Expr Expr
  | Mul Expr Expr
  | ...
```

---

## **Tedious recursion**

```haskell
subst :: Name -> Expr -> Expr -> Expr
subst v e expr =
  case expr of
    Literal i -> Literal i
    Var v' | v == v' -> e
    Var v' -> Var v'
    Add a b -> Add (subst v e a) (subst v e b)
    Mul a b -> Mul (subst v e a) (subst v e b)
    ...
```

---

## **`Expr` with recursion schemes**

```haskell
data Expr' e
  = Literal Int
  | Var Name
  | Add e e
  | Mul e e
  | ...
```

We use *open recursion*, and tie the knot with `Fix`:

```haskell
import Data.Fix(Fix)

type Expr = Fix Expr'
```

---

## **`subst` with recursion schemes**

```haskell
import Data.Functor.Foldable(cata)

subst :: Name -> Expr -> Expr -> Expr
subst v e = cata \case
  Var v' | v == v' -> e
  e' -> Fix e'
```

Recursion is abstracted away, we only mention what we actually care about

---

## ***Fin***
