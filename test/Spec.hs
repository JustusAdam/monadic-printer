{-# LANGUAGE OverloadedStrings #-}


import Test.Hspec
import Text.MonadicPrinter
import Data.ByteString (ByteString)
import Data.Text as T (Text, unlines)
import Data.Sequence
import Data.Foldable (toList)


combinerSpec :: Spec
combinerSpec = do
  describe "mempty" $
    it "produces an empty printer" $
      getLines mempty `shouldBe` (empty :: Seq [Char])

  describe "mappend" $ do
    it "combines with an empty printer" $ do
      getLines (return () <> "hello") `shouldBe` singleton ("hello" :: Text)
      getLines (mempty <> "hello") `shouldBe` singleton ("hello" :: Text)
      getLines (return () <> return ()) `shouldBe` (empty :: Seq Text)
      getLines ("hello" <> return ()) `shouldBe` singleton ("hello" :: Text)

    it "combines two printers by appending" $ do
      getLines ("hello" <> "you") `shouldBe` singleton ("helloyou" :: Text)
      (getLines $ do
        "hello"
        "you" <> " mate"
        ) `shouldBe` fromList ["hello", "you mate" :: Text]


converterSpec :: Spec
converterSpec = do
  describe "convertString" $ do
    it "converts String" $
      getLines (cs ("str" :: String)) `shouldBe` singleton ("str" :: Text)
    it "converts Text" $
      getLines (cs ("Text" :: Text)) `shouldBe` singleton ("Text" :: String)
    it "converts ByteString" $
      getLines (cs ("ByteString" :: ByteString)) `shouldBe` singleton ("ByteString" :: String)
  describe "convertObject" $ do
    it "converts String" $
      getLines (co ("Hello" :: String)) `shouldBe` singleton ("\"Hello\"" :: Text)
    it "converts Int" $
      getLines (co (12345 :: Int)) `shouldBe` singleton ("12345" :: ByteString)


retrieverSpec :: Spec
retrieverSpec = do
  let testObj = do
        "hello"
        "how are you"
        "pleased to see you"
      result = fromList ["hello", "how are you", "pleased to see you" :: Text]
  describe "getLines" $
    it "retrieves all lines contained as list" $
      getLines testObj `shouldBe` result

  describe "getText" $
    it "retrieves all contents as one text" $
      getText testObj `shouldBe` T.unlines (toList result)


main :: IO ()
main = hspec $ do
  retrieverSpec
  combinerSpec
  converterSpec
