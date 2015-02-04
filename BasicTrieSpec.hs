module BasicTrieSpec  (main, spec) where

import Data.List
import Test.Hspec
import Test.QuickCheck
import BasicTrie 

{-
quickCheck props
-}
insertedWordIsPresent :: String -> Bool
insertedWordIsPresent s = isWord s t == True
  where t = insertWord s emptyTrie

allPrefixesOfInsertedWordArePresent :: String -> Bool
allPrefixesOfInsertedWordArePresent s =  all (\p -> isPrefix p t) (inits s)
  where t = insertWord s emptyTrie
 

nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary


fromListAllWordsArePresent :: [String] -> Bool
fromListAllWordsArePresent words = all (\w -> isWord w t) words
  where t = fromList words

{-
spec runner
-}
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BasicTrie" $ do
    context "insert one word - 'bandana'" $ do
      let t = insertWord "bandana" emptyTrie
      it "should show 'bandana' is a word" $ do
        isWord "bandana" t `shouldBe` True 
      it "should show 'b', 'ban', and 'banda' are prefixes" $ do
        isPrefix "b" t `shouldBe` True
        isPrefix "ban" t `shouldBe` True
        isPrefix "banda" t `shouldBe` True
    context "QuickCheck assertions" $ do
      it "insertedWordIsPresent" $ do
        quickCheck insertedWordIsPresent
      it "allPrefixesOfInsertedWordArePresent" $ do
        quickCheck allPrefixesOfInsertedWordArePresent
      it "fromListAllWordsArePresent" $ do
        quickCheck $ forAll (listOf nonEmptyString) $ fromListAllWordsArePresent



