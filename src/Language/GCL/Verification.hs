module Language.GCL.Verification where
import Language.GCL.Syntax
import Language.GCL.Verification.Preprocessing(preprocess)
import Language.GCL.Verification.Wlp(runWLP)

verify :: Program -> Pred
verify = runWLP . preprocess

