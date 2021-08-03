module Lib where

import Constants    -- scrabble, bonus, handSize, vowels, consonants, vowelRatio
import Data.List as L                      -- disambiguate List.map from Map.map
import Data.Map.Strict as M                               -- needed for fromList
import System.Random                                     -- needed for randomRIO
import Control.Monad                                         -- needed for foldM

type Hand = Map Char Int                                -- Convenient type alias


-- PURE FUNCTIONS WITHOUT IO

-- example: turns "baseball" to
-- fromList [('b', 2), ('a', 2), ('s', 1), ('e', 1), ('l', 2)]
getFreqMap :: String -> Hand
getFreqMap word = let
    sortedWord = sortBy compare word                                   -- String
    groupedWord = groupBy (==) sortedWord                            -- [String]
    countBy str = ((nub str) !! 0, length str)          -- String -> (Char, Int)
    charCounts = L.map countBy groupedWord                      -- [(Char, Int)]
    in fromList charCounts                                               -- Hand


-- score is calculated as follows:
-- each letter score is looked up in scrabble, they are summed,
-- then the sum is multiplied by the length of the word.
-- if maximum number of letters were used, there is an added bonus.
getWordScore :: String -> Int -> Int
getWordScore word num = let
    letterScores = L.map (scrabble !) word                              -- [Int]
    extra = if (length word) == num then bonus else 0                     -- Int
    in (sum letterScores) * (length word) + extra                         -- Int


-- example: turns ('a', 3) to "a a a "
repeatChar :: (Char, Int) -> String                       -- used in displayHand
repeatChar (char, count) = concat (replicate count [char, ' '])


-- example: turns fromList [('b', 2), ('a', 2), ('s', 1), ('e', 1), ('l', 2)] to
-- "a a b b e l l s "
displayHand :: Hand -> String
displayHand hand = let
    repeatedLetters = L.map repeatChar (toList hand)                 -- [String]
    in concat repeatedLetters                                          -- String


-- returns True if all letters of word are in hand, and word is in dictionary.
isValidWord :: String -> Hand -> [String] -> Bool
isValidWord word hand dictionary = let
    wordHand = getFreqMap word                                           -- Hand
    pred = \char -> member char hand && (wordHand ! char) <= (hand ! char)
    in all pred word && elem word dictionary                             -- Bool


-- removes used letters from hand. Example: if the word played is "base", turns
-- fromList [('b', 2), ('a', 2), ('s', 1), ('e', 1), ('l', 2)] to
-- fromList [('b', 1), ('a', 1), ('s', 0), ('e', 0), ('l', 2)]
updateHand :: Hand -> String -> Hand
updateHand hand word = let
    wordHand = getFreqMap word                                           -- Hand
    fun letter count = case (wordHand !? letter) of        -- Char -> Int -> Int
        Just num -> count - num
        Nothing -> count
    in mapWithKey fun hand


-- IMPURE FUNCTIONS WITH IO:
concatIOstr :: IO String -> IO String -> IO String
concatIOstr ioStr1 ioStr2 = do                               -- used in dealHand
    x <- ioStr1
    y <- ioStr2
    return (x ++ y)


-- Example: takes "ab" and IO 'c', returns IO "abc"
foldFun :: String -> IO Char -> IO String               -- used in buildIOstring
foldFun str ioChar = (\x -> str ++ [x]) <$> ioChar                  -- IO String


-- given pool of letters to choose from, and a size, creates an IO string of
-- random selections from the pool. Example: "aeiou" 7 => IO "eaaeioo"
buildIOstring :: String -> Int -> IO String                  -- used in dealHand
buildIOstring pool size = let
    indices = [randomRIO(0, length pool - 1) | _ <- [1..size]]       -- [IO Int]
    chars = [(pool !!) <$> index | index <- indices]                -- [IO Char]
    in foldM foldFun "" chars                                       -- IO String


-- builds IO string of randomly selected vowels and consonants
dealHand :: Int -> Int -> IO String
dealHand size numVows = let
    vowelString = buildIOstring vowels numVows                      -- IO String
    consString = buildIOstring consonants (size - numVows)          -- IO String
    in concatIOstr vowelString consString                           -- IO String


-- opens and loads dictionary file into an IO list of strings
loadWords :: String -> IO [String]
loadWords file = lines <$> (readFile file)                        -- IO [String]


-- TODO
-- playHand :: Hand -> [String] -> Int -> IO ()
-- playGame :: [String] -> Int -> Int -> IO ()


someFunc :: IO ()
someFunc = do
    dictionary <- loadWords filename
    -- playGame dictionary handSize vowelRatio
    putStrLn "Game over! Thanks for playing."
