type A = ∀ a . (A a, ∀ a . A => A) => a -> ∀ a . a -> (A a) => A => A
