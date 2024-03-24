-- This checks that the layout of the alts is closed by the texp closer comma, and that its range only extends to the
-- end of the alt, not including the comment.
a =
  (
    case a of
      a -> a
        -- a
        ,
    1
  )
