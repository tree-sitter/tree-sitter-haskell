module A where

import Prelude
--     ^ module
import Data.Text
--     ^ module
--          ^ module

start :: Brokerage -> Game.World -> IO ThreadId
start broker world = do
  raws <- Raws.loadFromDhall
  -- <- variable
  forkIO
    . runRandomFaster rand
    -- ^ function
    --                 ^ variable
    . runTrace
    $ setup *> loop
