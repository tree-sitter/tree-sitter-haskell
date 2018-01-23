{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Node where

import Prelude
import Foreign
import Foreign.C
import GHC.Generics
import TreeSitter.Document

data Node = Node
  { nodeTSNode :: !TSNode
  , nodeType :: !CString
  , nodeSymbol :: !Word16
  , nodeStartPoint :: !TSPoint
  , nodeEndPoint :: !TSPoint
  , nodeStartByte :: !Int32
  , nodeEndByte :: !Int32
  , nodeNamedChildCount :: !Int32
  , nodeChildCount :: !Int32
  }
  deriving (Show, Eq, Generic)

data TSPoint = TSPoint { pointRow :: !Int32, pointColumn :: !Int32 }
  deriving (Show, Eq, Generic)

data TSNode = TSNode !(Ptr ()) !Int32 !Int32 !Int32
  deriving (Show, Eq, Generic)

peekAdvance :: forall a b. Storable a => Ptr a -> IO (a, Ptr b)
peekAdvance ptr = do
  let aligned = alignPtr ptr (alignment (undefined :: a))
  a <- peek aligned
  pure (a, castPtr aligned `plusPtr` sizeOf a)
{-# INLINE peekAdvance #-}

pokeAdvance :: forall a b. Storable a => Ptr a -> a -> IO (Ptr b)
pokeAdvance ptr a = do
  let aligned = alignPtr ptr (alignment (undefined :: a))
  poke aligned a
  pure (castPtr aligned `plusPtr` sizeOf a)
{-# INLINE pokeAdvance #-}

newtype Struct a = Struct { runStruct :: forall b . Ptr b -> IO (a, Ptr a) }

evalStruct :: Struct a -> Ptr b -> IO a
evalStruct = fmap (fmap fst) . runStruct

peekStruct :: forall a . Storable a => Struct a
peekStruct = Struct (\ p -> do
  let aligned = alignPtr (castPtr p) (alignment (undefined :: a))
  a <- peek aligned
  pure (a, aligned `plusPtr` sizeOf a))

pokeStruct :: Storable a => a -> Struct ()
pokeStruct a = Struct (\ p -> do
  let aligned = alignPtr (castPtr p) (alignment a)
  poke aligned a
  pure ((), castPtr aligned `plusPtr` sizeOf a))


instance Storable Node where
  alignment _ = alignment (TSNode nullPtr 0 0 0 :: TSNode)
  sizeOf _ = 72
  peek = evalStruct $ do
    nodeTSNode          <- peekStruct
    nodeType            <- peekStruct
    nodeSymbol          <- peekStruct
    nodeStartPoint      <- peekStruct
    nodeEndPoint        <- peekStruct
    nodeStartByte       <- peekStruct
    nodeEndByte         <- peekStruct
    nodeNamedChildCount <- peekStruct
    nodeChildCount      <- peekStruct
    pure $! Node nodeTSNode nodeType nodeSymbol nodeStartPoint nodeEndPoint nodeStartByte nodeEndByte nodeNamedChildCount nodeChildCount
  poke ptr (Node n t s sp ep sb eb nc c) = flip evalStruct ptr $ do
    pokeStruct n
    pokeStruct t
    pokeStruct s
    pokeStruct sp
    pokeStruct ep
    pokeStruct sb
    pokeStruct eb
    pokeStruct nc
    pokeStruct c

instance Storable TSPoint where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 8
  peek = evalStruct $ do
    pointRow    <- peekStruct
    pointColumn <- peekStruct
    pure $! TSPoint pointRow pointColumn
  poke ptr (TSPoint r c) = flip evalStruct ptr $ do
    pokeStruct r
    pokeStruct c

instance Storable TSNode where
  alignment _ = alignment (nullPtr :: Ptr ())
  sizeOf _ = 24
  peek = evalStruct $ do
    p  <- peekStruct
    o1 <- peekStruct
    o2 <- peekStruct
    o3 <- peekStruct
    pure $! TSNode p o1 o2 o3
  poke ptr (TSNode p o1 o2 o3) = flip evalStruct ptr $ do
    pokeStruct p
    pokeStruct o1
    pokeStruct o2
    pokeStruct o3


instance Functor Struct where
  fmap f a = Struct (\ p -> do
    (a', p') <- runStruct a p
    let fa = f a'
    fa `seq` pure (fa, castPtr p))

instance Applicative Struct where
  pure a = Struct (\ p -> pure (a, castPtr p))

  f <*> a = Struct (\ p -> do
    (f', p')  <- runStruct f          p
    (a', p'') <- runStruct a (castPtr p')
    let fa = f' a'
    fa `seq` pure (fa, castPtr p''))

instance Monad Struct where
  return = pure
  a >>= f = Struct (\ p -> do
    (a', p')   <- runStruct a               p
    (fa', p'') <- runStruct (f a') (castPtr p')
    pure (fa', p''))


foreign import ccall unsafe "src/bridge.c ts_document_root_node_p" ts_document_root_node_p :: Ptr Document -> Ptr Node -> IO ()

foreign import ccall unsafe "src/bridge.c ts_node_copy_named_child_nodes" ts_node_copy_named_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
foreign import ccall unsafe "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
