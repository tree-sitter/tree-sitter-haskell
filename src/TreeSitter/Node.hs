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

instance Functor Struct where
  fmap f (Struct run) = Struct (\ p -> do
    (a, p') <- run p
    let fa = f a
    fa `seq` pure (fa, castPtr p))


instance Storable Node where
  alignment _ = alignment (TSNode nullPtr 0 0 0 :: TSNode)
  sizeOf _ = 72
  peek ptr = do
    (nodeTSNode,          ptr) <- peekAdvance (castPtr ptr)
    (nodeType,            ptr) <- peekAdvance          ptr
    (nodeSymbol,          ptr) <- peekAdvance          ptr
    (nodeStartPoint,      ptr) <- peekAdvance          ptr
    (nodeEndPoint,        ptr) <- peekAdvance          ptr
    (nodeStartByte,       ptr) <- peekAdvance          ptr
    (nodeEndByte,         ptr) <- peekAdvance          ptr
    (nodeNamedChildCount, ptr) <- peekAdvance          ptr
    (nodeChildCount, _)        <- peekAdvance          ptr
    pure $! Node nodeTSNode nodeType nodeSymbol nodeStartPoint nodeEndPoint nodeStartByte nodeEndByte nodeNamedChildCount nodeChildCount
  poke ptr (Node n t s sp ep sb eb nc c) = do
    ptr <- pokeAdvance (castPtr ptr) n
    ptr <- pokeAdvance          ptr  t
    ptr <- pokeAdvance          ptr  s
    ptr <- pokeAdvance          ptr  sp
    ptr <- pokeAdvance          ptr  ep
    ptr <- pokeAdvance          ptr  sb
    ptr <- pokeAdvance          ptr  eb
    ptr <- pokeAdvance          ptr  nc
    _   <- pokeAdvance          ptr  c
    pure ()

instance Storable TSPoint where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 8
  peek ptr = do
    (pointRow,    ptr) <- peekAdvance (castPtr ptr)
    (pointColumn, _)   <- peekAdvance          ptr
    pure $! TSPoint pointRow pointColumn
  poke ptr (TSPoint r c) = do
    ptr <- pokeAdvance (castPtr ptr) r
    _   <- pokeAdvance          ptr  c
    pure ()


instance Storable TSNode where
  alignment _ = alignment (nullPtr :: Ptr ())
  sizeOf _ = 24
  peek ptr = do
    (p,  ptr) <- peekAdvance (castPtr ptr)
    (o1, ptr) <- peekAdvance          ptr
    (o2, ptr) <- peekAdvance          ptr
    (o3, _)   <- peekAdvance          ptr
    pure $! TSNode p o1 o2 o3
  poke ptr (TSNode p o1 o2 o3) = do
    ptr <- pokeAdvance (castPtr ptr) p
    ptr <- pokeAdvance          ptr  o1
    ptr <- pokeAdvance          ptr  o2
    _   <- pokeAdvance          ptr  o3
    pure ()



foreign import ccall unsafe "src/bridge.c ts_document_root_node_p" ts_document_root_node_p :: Ptr Document -> Ptr Node -> IO ()

foreign import ccall unsafe "src/bridge.c ts_node_copy_named_child_nodes" ts_node_copy_named_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
foreign import ccall unsafe "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
