a = do
  b
    -- a
    where c = d

a = do
  b
  where -- c
    d = e

a = do
  b
  where
    -- c
    d = e
