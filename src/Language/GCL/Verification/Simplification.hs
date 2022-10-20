module Language.GCL.Verification.Simplification(simplify) where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata)

import Language.GCL.Syntax ( Op(..), ExprF(..), Pred )
import Language.GCL.Syntax.Helpers ( pattern F, pattern T, pattern B, pattern I )

isCommutative :: Op -> Bool
isCommutative o = o `elem` [Add, Mul]

isAssociative :: Op -> Bool
isAssociative o = o `elem` [Add, Mul, And, Or]

simplify :: Pred -> Pred
simplify = cata go
  where
    go = \case
      Op And F _ -> F
      Op And T a -> a
      Op And _ F -> F
      Op And a T -> a

      Op Or T _ -> T
      Op Or F a -> a
      Op Or _ T -> T
      Op Or a F -> a

      Op Implies F _ -> T
      Op Implies T a -> a
      Op Implies _ T -> T
      Op Implies a F -> go $ Not a

      Not (B b) -> B $ not b
      Not (Fix (Not a)) -> a

      Negate (I i) -> I -i
      Negate (Fix (Negate a)) -> a

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

      Op o (Fix (Var a)) (Fix (Var a'))
        | a == a' && o `elem` [Eq, Lte, Gte] -> T
        | a == a' && o `elem` [Neq, Lt, Gt] -> F

      Op o (Fix (Op o' a b)) c
        | o == o' && isAssociative o -> go $ Op o a $ go $ Op o b c

      Op o a@I{} b
        | isCommutative o -> go $ Op o b a

      Op Sub (Fix (Op Sub e (I a))) (I b) -> go $ Op Sub e $ I $ a + b

      p -> Fix p
