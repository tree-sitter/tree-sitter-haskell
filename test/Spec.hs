import Foreign
import Foreign.C.Types
import Foreign.Storable
import Test.Hspec
import TreeSitter.Node

main :: IO ()
main = hspec $ do
  describe "TSNode" $ do
    it "has the same size as its C counterpart" $
      sizeOf (undefined :: TSNode) `shouldBe` fromIntegral sizeof_tsnode

    it "roundtrips correctly" $
      with (TSNode nullPtr 1 2) peek `shouldReturn` TSNode nullPtr 1 2

  describe "TSPoint" $ do
    it "has the same size as its C counterpart" $
      sizeOf (undefined :: TSPoint) `shouldBe` fromIntegral sizeof_tspoint

    it "roundtrips correctly" $
      with (TSPoint 1 2) peek `shouldReturn` TSPoint 1 2

  describe "Node" $ do
    it "has the same size as its C counterpart" $
      sizeOf (undefined :: Node) `shouldBe` fromIntegral sizeof_node

    it "roundtrips correctly" $
      with (Node (TSNode nullPtr 1 2) nullPtr 1 (TSPoint 2 3) (TSPoint 4 5) 6 7 8 9) peek `shouldReturn` Node (TSNode nullPtr 1 2) nullPtr 1 (TSPoint 2 3) (TSPoint 4 5) 6 7 8 9

foreign import ccall unsafe "src/bridge.c sizeof_tsnode" sizeof_tsnode :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_tspoint" sizeof_tspoint :: CSize
foreign import ccall unsafe "src/bridge.c sizeof_node" sizeof_node :: CSize
