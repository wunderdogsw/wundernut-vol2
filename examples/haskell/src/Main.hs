{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Monad (forM_)
import Data.Bits ((.|.), popCount)
import Data.Char (ord, toLower, toUpper)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM (empty, findWithDefault, fromList, insertWithKey, keysSet)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (nub, sort, sortOn)
import Data.Maybe ()
import Data.Set (Set)
import qualified Data.Set as S

finnishAlphabet :: String
finnishAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ"

finnishWordCharacters :: IntSet
finnishWordCharacters = IS.fromList $ map ord (finnishAlphabet ++ (map toLower finnishAlphabet) ++ "-")

powersOf2 :: [Int]
powersOf2 = map (\i -> 2 ^ i) [0..(length finnishAlphabet - 1)]

finnishCharToBitmaskMap :: IntMap Int
finnishCharToBitmaskMap = IM.fromList $
  zip (map ord finnishAlphabet) powersOf2 ++
  zip (map (ord . toLower) finnishAlphabet) powersOf2

bitmaskChar :: Char -> Int
bitmaskChar c = 
  IM.findWithDefault 0 (ord c) finnishCharToBitmaskMap

bitmaskWord :: String -> Int
bitmaskWord s = bitmaskWord' s 0
  where 
    bitmaskWord' :: String -> Int -> Int
    bitmaskWord' []     x = x
    bitmaskWord' (c:cs) x = bitmaskWord' cs (x .|. (bitmaskChar c))

prepareInput :: String -> IntMap [String] 
prepareInput input =
  let ws :: Set String
      ws = wordSet input
      wordMap :: IntMap [String]
      wordMap = S.foldl (\m w -> IM.insertWithKey (\_ new old -> old ++ new) (bitmaskWord w) [w] m) IM.empty ws
  in  wordMap

wordSet :: String -> Set String
wordSet string = wordSet' string S.empty
  where wordSet' s set = case dropWhile isNotFinnishWordCharacter s of
                                "" -> set
                                s' -> wordSet' s'' (S.insert w set)
                                  where (w, s'') = break isNotFinnishWordCharacter s'

iterativeMaxPopcountPairSearch :: IntSet -> [(Int, Int)]
iterativeMaxPopcountPairSearch intSet =
  let intsSortedDescByPopCount = reverse . sortOn popCount . IS.toList $ intSet
  in  iterateMaxPopcountPair intsSortedDescByPopCount (length finnishAlphabet)
  where iterateMaxPopcountPair :: [Int] -> Int -> [(Int, Int)]
        iterateMaxPopcountPair sortedInts cutoff =
          let pairsPotentiallyAboveCutoff = pairsWithCutoff sortedInts cutoff
          in  if null pairsPotentiallyAboveCutoff
              then iterateMaxPopcountPair sortedInts (cutoff - 1)
              else let pairsAboveCutoff = filter (\(a, b) -> popCount (a .|. b) == cutoff) pairsPotentiallyAboveCutoff
                   in if null pairsAboveCutoff
                      then iterateMaxPopcountPair sortedInts (cutoff - 1)
                      else pairsAboveCutoff
        pairsWithCutoff :: [Int] -> Int -> [(Int, Int)]
        pairsWithCutoff [] _       = []
        pairsWithCutoff _  0       = []
        pairsWithCutoff (_:[]) _   = []
        pairsWithCutoff (x:y:rs) n = 
          if (popCount x + popCount y < n)
          then []
          else map (x,) (takeWhile (\z -> popCount x + popCount z >= n) (y:rs)) ++ pairsWithCutoff (y:rs) n

findBestPairs :: String -> [([String], [String])]
findBestPairs input = 
  let bitmasksToWordListsMap :: IntMap [String]
      bitmasksToWordListsMap = prepareInput input
      uniqueBitmasks :: IntSet
      uniqueBitmasks = IM.keysSet bitmasksToWordListsMap
      maximumBitmaskPairsByPopCount :: [(Int, Int)]
      maximumBitmaskPairsByPopCount = iterativeMaxPopcountPairSearch uniqueBitmasks
      wordListsOfMaximumBitmaskPairs :: [([String], [String])]
      wordListsOfMaximumBitmaskPairs = 
        map (\(bm1, bm2) -> 
          (IM.findWithDefault 
            [] 
            bm1 
            bitmasksToWordListsMap, 
            IM.findWithDefault 
            [] 
            bm2 
            bitmasksToWordListsMap)) 
        maximumBitmaskPairsByPopCount
  in  sort wordListsOfMaximumBitmaskPairs

isNotFinnishWordCharacter :: Char -> Bool
isNotFinnishWordCharacter c = IS.notMember (ord c) finnishWordCharacters

main :: IO ()
main = do
  input <- getContents
  forM_ (findBestPairs input)
    (\(words1, words2) ->
      forM_ [(w1, w2) | w1 <- words1, w2 <- words2]
        (\(w1, w2) ->
          let coveredAlphabet = sort . nub . map toUpper $ (w1 ++ w2)
          in  putStrLn (w1 ++ ", " ++ w2 ++ " " ++ coveredAlphabet ++ " " ++ (show . length $ coveredAlphabet))))
