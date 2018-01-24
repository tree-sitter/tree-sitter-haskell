import Foreign.C.Types
import Foreign.Storable
import Test.Hspec
import TreeSitter.Node

main :: IO ()
main = hspec $ do
  describe "TSNode" $ do
    it "has the same size as its C counterpart" $ do
      sizeOf (undefined :: TSNode) `shouldBe` fromIntegral sizeof_tsnode

  describe "TSPoint" $ do
    it "has the same size as its C counterpart" $ do
      sizeOf (undefined :: TSPoint) `shouldBe` fromIntegral sizeof_tspoint

  describe "Node" $ do
    it "has the same size as its C counterpart" $ do
      sizeOf (undefined :: Node) `shouldBe` fromIntegral sizeof_node

foreign import ccall unsafe "src/bridge.c sizeof_tsnode" sizeof_tsnode :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tspoint" sizeof_tspoint :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_node" sizeof_node :: CSize
