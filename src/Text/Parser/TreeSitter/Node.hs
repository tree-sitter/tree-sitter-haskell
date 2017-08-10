{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Text.Parser.TreeSitter.Node where

import Prelude
import Foreign
import Foreign.C
import Foreign.CStorable
import GHC.Generics
import Text.Parser.TreeSitter.Document

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
  deriving (Show, Eq, Generic, CStorable)

data TSPoint = TSPoint { pointRow :: !Int32, pointColumn :: !Int32 }
  deriving (Show, Eq, Generic, CStorable)


data TSNode = TSNode !(Ptr ()) !Int32 !Int32 !Int32
  deriving (Show, Eq, Generic, CStorable)

peekAdvance :: forall a b. Storable a => Ptr a -> IO (a, Ptr b)
peekAdvance ptr = do
  let aligned = alignPtr ptr (alignment (undefined :: a))
  a <- peek aligned
  return (a, castPtr aligned `plusPtr` sizeOf a)
{-# INLINE peekAdvance #-}

pokeAdvance :: forall a b. Storable a => Ptr a -> a -> IO (Ptr b)
pokeAdvance ptr a = do
  let aligned = alignPtr ptr (alignment (undefined :: a))
  poke aligned a
  return (castPtr aligned `plusPtr` sizeOf a)
{-# INLINE pokeAdvance #-}

instance Storable Node where
  alignment _ = alignment (TSNode nullPtr 0 0 0 :: TSNode)
  sizeOf _ = 72
  peek ptr = do
    (nodeTSNode, ptr) <- peekAdvance (castPtr ptr)
    (nodeType, ptr) <- peekAdvance ptr
    (nodeSymbol, ptr) <- peekAdvance ptr
    (nodeStartPoint, ptr) <- peekAdvance ptr
    (nodeEndPoint, ptr) <- peekAdvance ptr
    (nodeStartByte, ptr) <- peekAdvance ptr
    (nodeEndByte, ptr) <- peekAdvance ptr
    (nodeNamedChildCount, ptr) <- peekAdvance ptr
    (nodeChildCount, _) <- peekAdvance ptr
    return $! Node nodeTSNode nodeType nodeSymbol nodeStartPoint nodeEndPoint nodeStartByte nodeEndByte nodeNamedChildCount nodeChildCount
  poke = cPoke

instance Storable TSPoint where
  alignment _ = alignment (0 :: Int32)
  sizeOf _ = 8
  peek ptr = do
    (pointRow, ptr) <- peekAdvance (castPtr ptr)
    (pointColumn, _) <- peekAdvance ptr
    return $! TSPoint pointRow pointColumn
  poke = cPoke

instance Storable TSNode where
  alignment _ = alignment (nullPtr :: Ptr ())
  sizeOf _ = 24
  peek ptr = do
    (p, ptr) <- peekAdvance (castPtr ptr)
    (o1, ptr) <- peekAdvance ptr
    (o2, ptr) <- peekAdvance ptr
    (o3, _) <- peekAdvance ptr
    return $! TSNode p o1 o2 o3
  poke ptr (TSNode p o1 o2 o3) = do
    ptr <- pokeAdvance (castPtr ptr) p
    ptr <- pokeAdvance ptr o1
    ptr <- pokeAdvance ptr o2
    _ <- pokeAdvance ptr o3
    return ()



foreign import ccall unsafe "src/bridge.c ts_document_root_node_p" ts_document_root_node_p :: Ptr Document -> Ptr Node -> IO ()

foreign import ccall unsafe "src/bridge.c ts_node_copy_named_child_nodes" ts_node_copy_named_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
foreign import ccall unsafe "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
