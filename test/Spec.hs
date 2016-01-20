{-# LANGUAGE OverloadedStrings #-}


import Test.Hspec
import Text.MonadicPrinter
import Data.ByteString (ByteString)
import Data.Text as T (Text, unlines)


combinerSpec :: Spec
combinerSpec = do
  describe "mempty" $
    it "produces an empty printer" $
      getLines mempty `shouldBe` []

  describe "mappend" $ do
    it "combines with an empty printer" $ do
      getLines (return () <> "hello") `shouldBe` ["hello"]
      getLines (mempty <> "hello") `shouldBe` ["hello"]
      getLines (return () <> return ()) `shouldBe` []
      getLines ("hello" <> return ()) `shouldBe` ["hello"]

    it "combines two printers by appending" $ do
      getLines ("hello" <> "you") `shouldBe` ["helloyou"]
      (getLines $ do
        "hello"
        "you" <> " mate"
        ) `shouldBe` ["hello", "you mate"]


converterSpec :: Spec
converterSpec = do
  describe "convertString" $ do
    it "converts String" $
      getLines (cs ("str" :: String)) `shouldBe` ["str"]
    it "converts Text" $
      getLines (cs ("Text" :: Text)) `shouldBe` ["Text"]
    it "converts ByteString" $
      getLines (cs ("ByteString" :: ByteString)) `shouldBe` ["ByteString"]
  describe "convertObject" $ do
    it "converts String" $
      getLines (co ("Hello" :: String)) `shouldBe` ["\"Hello\""]
    it "converts Int" $
      getLines (co (12345 :: Int)) `shouldBe` ["12345"]


retrieverSpec :: Spec
retrieverSpec = do
  let testObj = do
        "hello"
        "how are you"
        "pleased to see you"
      result = ["hello", "how are you", "pleased to see you"]
  describe "getLines" $
    it "retrieves all lines contained as list" $
      getLines testObj `shouldBe` result

  describe "getText" $
    it "retrieves all contents as one text" $
      getText testObj `shouldBe` T.unlines result


main :: IO ()
main = hspec $ do
  retrieverSpec
  combinerSpec
  converterSpec
