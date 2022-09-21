type Counter = Int

subst :: Id -> Expr -> Pred -> Pred
subst i e = para \case
  Var v | i == v -> e
  Length v | i == v -> e
  Forall v (p, _) | i == v -> Fix $ Forall v p
  Exists v (p, _) | i == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = Assert $ (¬)g
unroll n g s = If g (Seq s $ unroll (n - 1) g s) Skip


wlp :: Stmt -> Pred -> Pred
wlp Skip q = q
wlp (Assign i e) q = subst i e q
wlp (Seq s₁ s₂) q = wlp s₁ $ wlp s₂ q
wlp (Assert e) q = e ∧ q
wlp (Assume e) q = e ∨ q
wlp (If g s₁ s₂) q = (g ⟹ wlp s₁ q) ∧ ((¬)g ⟹ wlp s₂ q)
wlp (While g s) q = wlp (unroll 10 g s) q
wlp s _ = error $ "WLP not yet implemented for: " <> show (show s)

runWLP :: Program -> Pred
runWLP Program{..} = wlp programBody T