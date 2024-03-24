-- This checks that the `in` closes the `do` layouts after `pure a`, not including the comment in the `expression/do` nodes.
a = let a = do
          a do
            a <- a
            pure a
          -- a
          in a
