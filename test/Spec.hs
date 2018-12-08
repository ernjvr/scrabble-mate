import Test.Hspec
import Game
import Validator
import Factory

main :: IO ()
main = hspec $ do
  describe "score" $ do
    it "should calculate the correct score for given word and previous score" $ do
      score "hello" 5 `shouldBe` 10
      score "" 5 `shouldBe` 5

  describe "getScore" $ do
    it "should return the correct score based on a given result word and scoreboard" $ do
      getScore True "hello" 5 `shouldBe` 10
      getScore True "" 5 `shouldBe` 5
      getScore False "hello" 5 `shouldBe` 5
      getScore False "" 5 `shouldBe` 5

  describe "isGameComplete" $ do
    it "should return True if given round is 10 else it should return False" $ do
      isGameComplete 9 `shouldBe` False
      isGameComplete 10 `shouldBe` True
      isGameComplete 11 `shouldBe` False
      isGameComplete 01 `shouldBe` False

  describe "isWordInDictionary" $ do
    it "should return True if given word is in the dictionary else return False" $ do
      isWordInDictionary "hello" ["one", "two", "hello"] `shouldBe` True
      isWordInDictionary "HELLO" ["one", "two", "hello"] `shouldBe` True
      isWordInDictionary "Hello" ["one", "two", "hello"] `shouldBe` True
      isWordInDictionary "hello." ["one", "two", "hello"] `shouldBe` False
      isWordInDictionary "hello " ["one", "two", "hello"] `shouldBe` False
      isWordInDictionary "" ["one", "two", "hello"] `shouldBe` False

  describe "isAllCharsValid" $ do
    it "should return True if the all the given word's characters are found to have a corresponding character in the given random string else False" $ do
      isAllCharsValid "hello" "hello" `shouldBe` True
      isAllCharsValid "HELLO" "hello" `shouldBe` True
      isAllCharsValid "hello" "HELLO" `shouldBe` True