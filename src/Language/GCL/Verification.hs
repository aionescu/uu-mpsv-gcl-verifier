module Language.GCL.Verification(simplify, runWLP) where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata, para)

import Language.GCL.Syntax

simplify :: Pred -> Pred
simplify = cata go
  where
    go = \case
      BinOp And F _ -> F
      BinOp And T a -> a
      BinOp Or T _ -> T
      BinOp Or F a -> a

      BinOp And _ F -> F
      BinOp And a T -> a
      BinOp Or _ T -> T
      BinOp Or a F -> a

      BinOp Implies F _ -> T
      BinOp Implies T a -> a
      BinOp Implies _ T -> T
      BinOp Implies a F -> go $ Negate a

      Negate (B b) -> B $ not b
      Negate (I i) -> I $ -i

      BinOp o (I a) (I b)
        | Add <- o -> I (a + b)
        | Sub <- o -> I (a - b)
        | Mul <- o -> I (a * b)
        | Div <- o -> I (a `quot` b)

        | Eq <- o -> B (a == b)
        | Neq <- o -> B (a /= b)
        | Lt <- o -> B (a < b)
        | Lte <- o -> B (a <= b)
        | Gt <- o -> B (a > b)
        | Gte <- o -> B (a >= b)

      BinOp o (Fix (Var a)) (Fix (Var a'))
        | a == a' && o `elem` [Eq, Lte, Gte] -> T
        | a == a' && o `elem` [Neq, Lt, Gt] -> F

      BinOp o (Fix (BinOp o' a b)) c
        | o == o' && isAssociative o -> go $ BinOp o a $ Fix $ BinOp o b c

      BinOp o a@I{} b
        | isCommutative o -> go $ BinOp o b a

      p -> Fix p

subst :: Id -> Expr -> Pred -> Pred
subst i e = para \case
  Var v | i == v -> e
  Length v | i == v -> e
  Forall v (p, _) | i == v -> Fix $ Forall v p
  Exists v (p, _) | i == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = Assert (-g)
unroll n g s = If g (Seq s $ unroll (n - 1) g s) Skip

wlp :: Stmt -> Pred -> Pred
wlp Skip q = q
wlp (Assign i e) q = subst i e q
wlp (Seq s₁ s₂) q = wlp s₁ $ wlp s₂ q
wlp (Assert e) q = e :&& q
wlp (Assume e) q = e :=> q
wlp (If g s₁ s₂) q = (g :=> wlp s₁ q) :&& (-g :=> wlp s₂ q)
wlp (While g s) q = wlp (unroll 10 g s) q
wlp _ _ = error "wlp: TODO"

runWLP :: Program -> Pred
runWLP Program{..} = wlp programBody T
