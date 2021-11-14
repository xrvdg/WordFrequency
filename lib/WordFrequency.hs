{-# LANGUAGE OverloadedStrings #-}

module WordFrequency
  ( WordFrequency,
    WordFrequency.words,
    createFrequencyMap,
    calculateHighestFrequency,
    calculateMostFrequentNWords,
    calculateFrequencyForWord,
    downUp,
  )
where

import qualified Data.HashMap.Strict as H
import Data.List (sortBy)
import Data.Ord (Down (..))
import qualified Data.Text as T

type WordFrequency = (Int, T.Text)

{- In the problem descriptions words are defined as a sequence of characters in the range of a-z and A-z.
Therefore we split the input text on every non alphabetic character.-}
words :: T.Text -> [T.Text]
words = removeEmpty . T.split (\c -> not (between ('a', 'z') c || between ('A', 'Z') c))
  where
    removeEmpty = filter (/= "") -- Remove empty strings that are the result of consecutive non-alphebetic characters.
    between (lower, upper) c = lower <= c && c <= upper

calculateFrequencyForWord :: T.Text -> T.Text -> Int
calculateFrequencyForWord text word = length . keep word . WordFrequency.words . T.toLower $ text
  where
    keep t = filter (== T.toLower t)

calculateHighestFrequency :: T.Text -> Int
calculateHighestFrequency = maximum . map fst . countOccurencesHashMap

calculateMostFrequentNWords :: T.Text -> Int -> [WordFrequency]
calculateMostFrequentNWords text n = take n . sortBy downUp $ countOccurencesHashMap text

countOccurencesHashMap :: T.Text -> [WordFrequency]
countOccurencesHashMap = map swap . H.toList . createFrequencyMap

createFrequencyMap :: T.Text -> H.HashMap T.Text Int
createFrequencyMap text = H.fromListWith (+) kv
  where
    t = WordFrequency.words (T.toLower text)
    kv = zip t (repeat 1)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- Helper function to sort descending on the first element and ascending on the second element.
downUp :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
downUp (ai, at) (bi, bt) = case compare (Down ai) (Down bi) of
  EQ -> compare at bt
  result -> result
