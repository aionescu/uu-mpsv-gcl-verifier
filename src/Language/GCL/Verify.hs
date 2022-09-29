module Language.GCL.Verify(verify) where
import Language.GCL.Syntax
import Language.GCL.Verification.Preprocessing(preprocess)
import Language.GCL.Verification.Wlp(runWLP)


verify :: Program -> Pred
verify = runWLP . preprocess
