module Language.GCL.Utils where

import Data.Text(Text)
import Data.Text qualified as T

showT :: Show a => a -> Text
showT = T.pack . show

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)
