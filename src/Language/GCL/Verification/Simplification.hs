module Language.GCL.Verification.Simplification(simplify) where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata)

import Language.GCL.Syntax ( BinOp(..), ExprF(BinOp, Negate, Var), Pred )
import Language.GCL.Syntax.Helpers ( pattern F, pattern T, pattern B, pattern I ) 


isCommutative :: BinOp -> Bool
isCommutative o = o `elem` [Add, Mul]

isAssociative :: BinOp -> Bool
isAssociative o = o `elem` [Add, Mul, And, Or]

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
      Negate (I i) -> I -i

      BinOp o (I a) (I b)
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

      BinOp o (Fix (Var a)) (Fix (Var a'))
        | a == a' && o `elem` [Eq, Lte, Gte] -> T
        | a == a' && o `elem` [Neq, Lt, Gt] -> F

      BinOp o (Fix (BinOp o' a b)) c
        | o == o' && isAssociative o -> go $ BinOp o a $ go $ BinOp o b c

      BinOp o a@I{} b
        | isCommutative o -> go $ BinOp o b a

      p -> Fix p