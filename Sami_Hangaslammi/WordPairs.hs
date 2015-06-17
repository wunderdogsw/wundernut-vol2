module Main where

import Data.List
import Data.Char     (ord, chr, isAlpha)
import Data.Function (on)
import Data.Ord      (comparing, Down(..))

import Data.Set    (Set)
import Data.IntSet (IntSet)
import qualified Data.Set        as Set
import qualified Data.IntSet     as IntSet
import qualified Data.Map.Strict as Map

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TL

type Letters   = IntSet
type Tagged a  = (Letters, a)
type Words     = Set Text

main :: IO ()
main
    =   TL.getContents
    >>= print
    .   PrettyPairs
    .   pickLargest
    .   everyPair
    .   filterSubsets
    .   splitWords

-- Pick all elements that have the largest number of unique letters
pickLargest :: [Tagged a] -> [Tagged a]
pickLargest
    = head
    . groupBy ((==) `on` largest)
    . sortBy (comparing largest)
    where
        largest = Down . IntSet.size . fst

-- Generate all possible element pairings
everyPair :: [Tagged a] -> [Tagged (a, a)]
everyPair ws = do
    (w : ws') <- tails ws
    fmap (mkPair w) ws'
    where
        mkPair (la, a) (lb, b) = (IntSet.union la lb, (a, b))

-- Filter out words that are a proper subset of some other word
filterSubsets :: [Tagged a] -> [Tagged a]
filterSubsets sets = filter hasNoSuperSet sets where
    hasNoSuperSet (s, _) = not $ any (\(p, _) -> s `IntSet.isProperSubsetOf` p) sets

-- Split a piece of text into words grouped by their unique letters
splitWords :: Text -> [Tagged Words]
splitWords
    = Map.toList
    . Map.fromListWith Set.union
    . decorate letters
    . map (TL.filter isAlpha . TL.toLower)
    . TL.words

decorate :: Ord a => (a -> d) -> [a] -> [(d, Set a)]
decorate f = map (\a -> (f a, Set.singleton a))

letters :: Text -> Letters
letters = IntSet.fromList . map ord . TL.unpack


-- Pretty printing for result
newtype PrettyPairs = PrettyPairs [Tagged (Words, Words)]

instance Show PrettyPairs where
    show (PrettyPairs wps) = unlines $ map formatPair wps where
        formatPair (l, (a, b)) = concat [formatSet l, " : ", formatWords a, ", ", formatWords b]
        formatWords = intercalate "|" . map TL.unpack . Set.toList
        formatSet s = concat ["{", (intercalate "," $ map (return . chr) $ IntSet.toList s), "}"]

