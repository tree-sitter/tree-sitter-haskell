{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module TreeSitter.Node
( Node(..)
, TSPoint(..)
, TSNode(..)
, ts_document_root_node_p
, ts_node_copy_named_child_nodes
, ts_node_copy_child_nodes
) where

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


-- | 'Struct' is a strict 'Monad' with automatic alignment & advancing, & inferred type.
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
  peek = evalStruct $ Node <$> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
                           <*> peekStruct
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
  peek = evalStruct $ TSPoint <$> peekStruct
                              <*> peekStruct
  poke ptr (TSPoint r c) = flip evalStruct ptr $ do
    pokeStruct r
    pokeStruct c

instance Storable TSNode where
  alignment _ = alignment (nullPtr :: Ptr ())
  sizeOf _ = 24
  peek = evalStruct $ TSNode <$> peekStruct
                             <*> peekStruct
                             <*> peekStruct
                             <*> peekStruct
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
  {-# INLINE fmap #-}

instance Applicative Struct where
  pure a = Struct (\ p -> pure (a, castPtr p))
  {-# INLINE pure #-}

  f <*> a = Struct (\ p -> do
    (f', p')  <- runStruct f          p
    (a', p'') <- runStruct a (castPtr p')
    let fa = f' a'
    fa `seq` pure (fa, castPtr p''))
  {-# INLINE (<*>) #-}

instance Monad Struct where
  return = pure
  {-# INLINE return #-}

  a >>= f = Struct (\ p -> do
    (a', p')   <- runStruct a               p
    (fa', p'') <- runStruct (f a') (castPtr p')
    pure (fa', p''))
  {-# INLINE (>>=) #-}


foreign import ccall unsafe "src/bridge.c ts_document_root_node_p" ts_document_root_node_p :: Ptr Document -> Ptr Node -> IO ()

foreign import ccall unsafe "src/bridge.c ts_node_copy_named_child_nodes" ts_node_copy_named_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
foreign import ccall unsafe "src/bridge.c ts_node_copy_child_nodes" ts_node_copy_child_nodes :: Ptr Document -> Ptr TSNode -> Ptr Node -> CSize -> IO ()
