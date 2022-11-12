module Language.GCL.Verification.Simplification(simplify) where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata)

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers

simplify :: Pred -> Pred
simplify = cata go
  where
    go = \case
      Not (B b) -> B $ not b
      Not (Not' a) -> a
      Not (Forall' v e) -> go $ Exists v $ go $ Not e
      Not (Exists' v e) -> go $ Forall v $ go $ Not e

      Not (Op' o a b)
        | And <- o -> go $ Op Or (go $ Not a) $ go $ Not b
        | Or <- o -> go $ Op And (go $ Not a) $ go $ Not b
        | Eq <- o -> go $ Op Neq a b
        | Neq <- o -> go $ Op Eq a b
        | Lt <- o -> go $ Op Gte a b
        | Lte <- o -> go $ Op Gt a b
        | Gt <- o -> go $ Op Lte a b
        | Gte <- o -> go $ Op Lt a b

      Negate (I i) -> I -i
      Negate (Negate' a) -> a

      Negate (Op' Add a b) -> go $ Op Add (go $ Negate a) $ go $ Negate b
      Negate (Op' Mul a b) -> go $ Op Mul a $ go $ Negate b

      Forall _ b@B{} -> b
      Exists _ b@B{} -> b

      Cond T a _ -> a
      Cond F _ a -> a

      Subscript (RepBy' v i e) i'
        | I a <- i, I b <- i', a /= b -> go $ Subscript v i'
        | i == i' -> e

      RepBy (RepBy' v i _) i' e | i == i' -> go $ RepBy v i e

      Op Implies a b -> go $ Op Or (go $ Not a) b

      Op And F _ -> F
      Op And T a -> a
      Op And _ F -> F
      Op And a T -> a

      Op Or T _ -> T
      Op Or F a -> a
      Op Or _ T -> T
      Op Or a F -> a

      Op Eq T a -> a
      Op Eq F a -> go $ Not a
      Op Eq a T -> a
      Op Eq a F -> go $ Not a
      Op Eq Null' Null' -> T

      Op Neq T a -> go $ Not a
      Op Neq F a -> a
      Op Neq a T -> go $ Not a
      Op Neq a F -> a
      Op Neq Null' Null' -> F

      Op o (I a) (I b)
        | Add <- o -> I $ a + b
        | Sub <- o -> I $ a - b
        | Mul <- o -> I $ a * b
        | Div <- o -> I $ a `quot` b

        | Eq <- o -> B $ a == b
        | Neq <- o -> B $ a /= b
        | Lt <- o -> B $ a < b
        | Lte <- o -> B $ a <= b
        | Gt <- o -> B $ a > b
        | Gte <- o -> B $ a >= b

      Op o (Var' a) (Var' a')
        | a == a' && o `elem` [Eq, Lte, Gte] -> T
        | a == a' && o `elem` [Neq, Lt, Gt] -> F
        | a == a', Add <- o -> go $ Op Mul (Fix $ Var a) $ I 2
        | a == a', Sub <- o -> I 0
        | a == a', Div <- o -> I 1

      Op o (Op' o' a b) c
        | o == o' && o `elem` [Add, Mul, And, Or] -> go $ Op o a $ go $ Op o b c
        | Add <- o', o `elem` [Eq, Neq, Lt, Lte, Gt, Gte] -> go $ Op o a $ go $ Op Sub c b

      Op o a@I{} b
        | o `elem` [Add, Mul, Eq, Neq] -> go $ Op o b a
        | Lt <- o -> go $ Op Gt b a
        | Lte <- o -> go $ Op Gte b a
        | Gt <- o -> go $ Op Lt b a
        | Gte <- o -> go $ Op Lte b a

      Op Sub a b -> go $ Op Add a $ go $ Negate b

      Op Div (Op' Div a b) c -> go $ Op Div a $ go $ Op Mul b c

      p -> Fix p
