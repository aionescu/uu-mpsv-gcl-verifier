module Language.GCL.Verify(verify) where
import Language.GCL.Syntax
import Language.GCL.Verification.Preprocessing(preprocess)
import Language.GCL.Verification.Wlp(runWlp)


verify :: Program -> Pred
verify = runWlp . preprocess
