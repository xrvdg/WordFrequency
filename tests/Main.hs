{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.HashMap.Strict as H
import Data.List (sort, sortBy)
import qualified Data.Proxy as HashMap
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec as HSpec
import WordFrequency

{-
Test functions by using different underlying approaches. (confluence property)
-}
calculateHighestFrequency :: T.Text -> Int
calculateHighestFrequency = maximum . map fst . countOccurencesList

countOccurencesList :: T.Text -> [WordFrequency]
countOccurencesList = foldr f [] . sort . WordFrequency.words . T.toLower
  where
    f :: T.Text -> [WordFrequency] -> [WordFrequency]
    f t xss@((n, x) : xs) = if t == x then (n + 1, x) : xs else (1, t) : xss
    f t [] = [(1, t)]

calculateFrequencyForWord :: T.Text -> T.Text -> Int
calculateFrequencyForWord text word = H.findWithDefault 0 (T.toLower word) (createFrequencyMap text)

calculateMostFrequentNWords :: T.Text -> Int -> [WordFrequency]
calculateMostFrequentNWords t n = take n . sortBy downUp . countOccurencesList $ t

main :: IO ()
main = do
  frank <- TIO.readFile "./tests/frankenstein.txt"
  hspec $ do
    HSpec.describe "test" $ do
      it "same result for HashMap- and List-based most frequent words" $ do
        Main.calculateMostFrequentNWords frank 20 `shouldBe` WordFrequency.calculateMostFrequentNWords frank 20
      it "same result for HashMap- and List-based frequency for word" $ do
        Main.calculateFrequencyForWord frank "have" `shouldBe` WordFrequency.calculateFrequencyForWord frank "have"
      it "same result for HashMap- and List-based hightest frequency" $ do
        Main.calculateHighestFrequency frank `shouldBe` WordFrequency.calculateHighestFrequency frank
      it "calculateFrequencyForWord should be insensitive for the case of the lookup word" $ do
        WordFrequency.calculateFrequencyForWord frank "have" `shouldBe` WordFrequency.calculateFrequencyForWord frank "HaVe"
