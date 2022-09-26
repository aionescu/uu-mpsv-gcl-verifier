{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Language.GCL.Verification(runWLP) where
import Language.GCL.Syntax
import Control.Monad.State.Strict(State, get, put, evalState)
import Language.GCL.Utils ( showT )

type Counter = Int

subst :: Id -> (Expr -> Expr) -> Pred -> Pred
subst _ _ l@(IntLit _) = l
subst _ _ l@(BoolLit _) = l
subst i fe v@(Var x)
  | i == x = fe v
  | otherwise = v
subst i fe v@(Length x)
  | i == x = fe v
  | otherwise = v
subst i fe (BinOp op lhs rhs) = BinOp op (subst i fe lhs) (subst i fe rhs)
subst i fe (Negate rhs) = Negate $ subst i fe rhs
subst i fe (Subscript v s)
  | i == v = fe nst
  | otherwise = Subscript v nst
  where nst = subst i fe s
subst i fe f@(Forall v p)
  | i == v = f
  | otherwise = Forall v $ subst i fe p
subst i fe f@(Exists v p)
  | i == v = f
  | otherwise = Exists v $ subst i fe p
subst i fe (Conditional c e1 e2) = Conditional (tr c) (tr e1) (tr e2)
  where tr = subst i fe

-- repby :: Expr -> Pred -> Pred
-- repby s@(Subscript i e) = s

wlp :: Stmt -> Pred -> Pred
wlp Skip q = q
wlp (Assign i e) q = subst i (const e) q
wlp (Seq s₁ s₂) q = wlp s₁ $ wlp s₂ q
wlp (Assert e) q = e :&& q
wlp (Assume e) q = e :=> q
wlp (If g s₁ s₂) q = (g :=> wlp s₁ q) :&& (-g :=> wlp s₂ q)
-- wlp (AssignIndex ad i as) q = 
--   where repby = Conditional 
wlp (While _ _) _ = error "Loops not allowed in WLP"
wlp _ _ = error "wlp: TODO"

runWLP :: Program -> Pred
runWLP Program{..} = wlp programBody $ BoolLit True


preprocess :: Program -> Program
preprocess = runRemoveShadowing . unrollLoops  

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = Assert (-g)
unroll n g s = If g (Seq s $ unroll (n - 1) g s) Skip


unrollLoops :: Program -> Program
unrollLoops Program{..} =
  case programBody of
    -- recursion schemes
    (While g s) -> Program{programBody=unroll 10 g s, ..}
    _ -> Program{..}

fresh :: Id -> State Counter Id 
fresh name = do
  c <- get
  put $ c + 1
  return $ "$" <> name <> (showT c)

substStmt :: Id -> Id -> Stmt -> Stmt
substStmt id nid (Assign i e) = Assign cid $ subst id (const (Var nid)) e
  where cid = if i == id then nid else i
substStmt id nid (Seq s1 s2) = Seq l r
  where l = substStmt id nid s2
        r = substStmt id nid s1
substStmt id nid (Assert e) = Assert $ subst id (const (Var nid)) e
substStmt id nid (Assume e) = Assume $ subst id (const (Var nid)) e
substStmt id nid (AssignIndex i e1 e2) = AssignIndex cid (subst id (const (Var nid)) e1) (subst id (const (Var nid)) e2)
  where cid = if i == id then nid else i
substStmt id nid (Let _ s) = substStmt id nid s
substStmt id nid (If e s1 s2) = If ne ns1 ns2
  where ne = subst id (const (Var nid)) e
        ns1 = substStmt id nid s1
        ns2 = substStmt id nid s2
substStmt id nid (While e s1) = While ne ns1
  where ne = subst id (const (Var nid)) e
        ns1 = substStmt id nid s1
substStmt _ _ e = e


removeShadowingStmt :: Stmt -> State Counter Stmt
removeShadowingStmt (Let dcs s) = do
  ns <- removeShadowingStmt s
  let names = map (\Decl{..} -> declName) dcs
  tr <- mapM fresh names
  let nD = map (\(Decl{..}, n) -> Decl{declName=n, declType}) $ zip dcs tr
  let nS = foldl (flip $ uncurry substStmt) ns $ zip names tr
  return $ Let nD nS
removeShadowingStmt (If e s1 s2) = do
    ns1 <- removeShadowingStmt s1
    ns2 <- removeShadowingStmt s2
    return $ If e ns1 ns2
removeShadowingStmt (While e s) = do
    ns <- removeShadowingStmt s
    return $ While e ns
removeShadowingStmt (Seq s1 s2) = do
    ns1 <- removeShadowingStmt s1
    ns2 <- removeShadowingStmt s2
    return $ Seq ns1 ns2
removeShadowingStmt s = return s


removeShadowing :: Program -> State Counter Program
removeShadowing Program{..} = do
  programBody <- removeShadowingStmt programBody
  return Program{..}

runRemoveShadowing :: Program -> Program
runRemoveShadowing p = evalState (removeShadowing p) 0

verify :: Program -> Pred
verify = runWLP . preprocess



