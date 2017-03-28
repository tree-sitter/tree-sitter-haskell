{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Text.Parser.TreeSitter.Node where

import Foreign
import Foreign.C
import Foreign.CStorable
import GHC.Generics
import Text.Parser.TreeSitter.Document

data Node = Node
  { nodeTSNode :: !TSNode
  , nodeType :: !CString
  , nodeStartPoint :: !Point
  , nodeEndPoint :: !Point
  , nodeStartByte :: !Int32
  , nodeEndByte :: !Int32
  , nodeNamedChildCount :: !Int32
  , nodeChildCount :: !Int32
  }
  deriving (Show, Eq, Generic, CStorable)

data Point = Point { pointRow :: !Int32, pointColumn :: !Int32 }
  deriving (Show, Eq, Generic, CStorable)


data TSNode = TSNode !(Ptr ()) !Int32 !Int32 !Int32
  deriving (Show, Eq, Generic, CStorable)


instance Storable Node where
  alignment = cAlignment
  sizeOf = cSizeOf
  peek = cPeek
  poke = cPoke

instance Storable Point where
  alignment = cAlignment
  sizeOf = cSizeOf
  peek = cPeek
  poke = cPoke

instance Storable TSNode where
  alignment = cAlignment
  sizeOf = cSizeOf
  peek = cPeek
  poke = cPoke


foreign import ccall "src/bridge.c ts_document_root_node_p" ts_document_root_node_p :: Ptr Document -> Ptr Node -> IO ()

foreign import ccall "src/bridge.c ts_node_copy_named_child_nodes" ts_node_copy_named_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
foreign import ccall "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
