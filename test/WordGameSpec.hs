module WordGameSpec where

import Test.Hspec
import Test.QuickCheck
import Lib
import Constants
import Data.Map.Strict

spec :: Spec
spec = do
    describe "getFreqMap" $ do
        it "returns empty map when given empty string" $ property $
            getFreqMap "" == fromList []

        it "returns correct map when given string 'baseball'" $ property $
            getFreqMap "baseball" == fromList [('a',2),('b',2),('e',1),('l',2),('s',1)]

    describe "getWordScore" $ do
        it "returns 0 when given empty string" $ property $
            getWordScore "" handSize == 0

        it "returns correct score when given 'bazinga' 10" $ property $
            getWordScore "bazinga" handSize == 133

        it "returns correct score WITH BONUS when given 'bazinga' 7" $ property $
            getWordScore "bazinga" 7 == 183

        it "returns correct score when given 'it' 7" $ property $
            getWordScore "it" 7 == 4

        it "returns correct score when given 'was' 7" $ property $
            getWordScore "was" 7 == 18

        it "returns correct score when given 'fork' 7" $ property $
            getWordScore "fork" 7 == 44

        it "returns correct score when given 'scored' 7" $ property $
            getWordScore "scored" 7 == 54

        it "returns correct score WITH BONUS when given 'outgnaw' 7" $ property $
            getWordScore "outgnaw" 7 == 127

        it "returns correct score WITH BONUS when given 'waybill' 7" $ property $
            getWordScore "waybill" 7 == 155

    describe "displayHand" $ do
        it "returns empty string given empty hand" $ property $
            displayHand(fromList []) == ""

        it "returns correct string given hand for 'baseball'" $ property $
            displayHand(getFreqMap "baseball") == "a a b b e l l s "

    describe "isValidWord" $ do
        it "returns False given empty dictionary" $ property $
            isValidWord "base" (getFreqMap "baseball") [] == False

        it "returns False given empty hand" $ property $
            isValidWord "base" (fromList []) ["base", "ball", "baseball"] == False

        it "returns True given hand and dictionary that contain word" $ property $
            isValidWord "base" (getFreqMap "baseball") ["base", "ball", "baseball"] == True

        it "returns False given hand that does not contain word" $ property $
            isValidWord "rapture" (getFreqMap "raptue") ["rapture"] == False

        it "returns False given dictionary that does not contain word" $ property $
            isValidWord "rapture" (getFreqMap "rapture") ["baseball"] == False

    describe "updateHand" $ do
        it "returns empty hand given empty hand" $ property $
            updateHand (fromList []) "base" == (fromList [])

        it "returns same hand given empty word" $ property $
            updateHand (getFreqMap "baseball") "" == getFreqMap "baseball"

        it "returns correctly updated hand for hand: 'quaillm' word: 'quail'" $ property $
            updateHand (getFreqMap "quaillm") "quail" == fromList [
                ('q', 0), ('u', 0), ('a', 0), ('i', 0), ('l', 1), ('m', 1)
            ]

        it "returns correctly updated hand for hand: 'evvilln' word: 'evil'" $ property $
            updateHand (getFreqMap "evvilln") "evil" == fromList [
                ('e', 0), ('v', 1), ('i', 0), ('l', 1), ('n', 1)
            ]
