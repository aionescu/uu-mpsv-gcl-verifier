module Language.GCL.Verification(verify) where

import Language.GCL.Syntax
import Language.GCL.Verification.Preprocessing(preprocess)
import Language.GCL.Verification.WLP(runWLP)

verify :: Program -> Pred
verify = runWLP . preprocess
