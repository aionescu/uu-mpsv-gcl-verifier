module Language.GCL.Utils where

import Data.Text(Text)
import Data.Text qualified as T
import Text.Printf(printf)

showT :: Show a => a -> Text
showT = T.pack . show

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)

ratio :: Int -> Int -> String
ratio a b = printf "%d/%d (%.2f%%)" a b $ fromIntegral  a * (100 :: Double) / fromIntegral b
