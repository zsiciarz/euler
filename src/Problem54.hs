
module Problem54 where

import Control.Applicative ((<$>), liftA2)
import Control.Arrow ((***))
import Data.Function (on)
import Data.List (isInfixOf, groupBy, sort)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import System.IO (IOMode(..), withFile, hGetContents)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Show, Read)

charToSuit :: Char -> Maybe Suit
charToSuit 'C' = Just Clubs
charToSuit 'D' = Just Diamonds
charToSuit 'H' = Just Hearts
charToSuit 'S' = Just Spades
charToSuit _ = Nothing

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Bounded, Enum, Show, Read)

charToRank :: Char -> Maybe Rank
charToRank '2' = Just Two
charToRank '3' = Just Three
charToRank '4' = Just Four
charToRank '5' = Just Five
charToRank '6' = Just Six
charToRank '7' = Just Seven
charToRank '8' = Just Eight
charToRank '9' = Just Nine
charToRank 'T' = Just Ten
charToRank 'J' = Just Jack
charToRank 'Q' = Just Queen
charToRank 'K' = Just King
charToRank 'A' = Just Ace
charToRank _ = Nothing

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Eq, Show, Read)

instance Ord Card where
    compare = compare `on` rank

stringToCard :: String -> Maybe Card
stringToCard [r, s] = liftA2 Card (charToRank r) (charToSuit s)
stringToCard _ = Nothing

data Hand = RoyalFlush
          | StraightFlush Rank
          | Quads Rank Rank
          | FullHouse Rank Rank
          | Flush Rank
          | Straight Rank
          | ThreeOfAKind Rank [Rank]
          | TwoPairs Rank Rank Rank
          | Pair Rank [Rank]
          | HighCard Rank [Rank]
          deriving (Eq, Show, Read)

instance Ord Hand where
    RoyalFlush `compare` RoyalFlush = EQ
    RoyalFlush `compare` _ = GT
    _ `compare` RoyalFlush = LT

    StraightFlush r1 `compare` StraightFlush r2 = r1 `compare` r2
    StraightFlush _ `compare` _ = GT
    _ `compare` StraightFlush _ = LT

    Quads r1 rest1 `compare` Quads r2 rest2 =
        (r1 `compare` r2) <> (rest1 `compare` rest2)
    Quads _ _ `compare` _ = GT
    _ `compare` Quads _ _ = LT

    FullHouse triple1 double1 `compare` FullHouse triple2 double2 =
        (triple1 `compare` triple2) <> (double1 `compare` double2)
    FullHouse _ _ `compare` _ = GT
    _ `compare` FullHouse _ _ = LT

    Flush r1 `compare` Flush r2 = r1 `compare` r2
    Flush _ `compare` _ = GT
    _ `compare` Flush _ = LT

    Straight r1 `compare` Straight r2 = r1 `compare` r2
    Straight _ `compare` _ = GT
    _ `compare` Straight _ = LT

    ThreeOfAKind r1 rest1 `compare` ThreeOfAKind r2 rest2 =
        (r1 `compare` r2) <> (compare `on` reverse) rest1 rest2
    ThreeOfAKind _ _ `compare` _ = GT
    _ `compare` ThreeOfAKind _ _ = LT

    TwoPairs high1 low1 rest1 `compare` TwoPairs high2 low2 rest2 =
        (high1 `compare` high2) <> (low1 `compare` low2) <> (rest1 `compare` rest2)
    TwoPairs _ _ _ `compare` _ = GT
    _ `compare` TwoPairs _ _ _ = LT

    Pair r1 rest1 `compare` Pair r2 rest2 =
        (r1 `compare` r2) <> (compare `on` reverse) rest1 rest2
    Pair _ _ `compare` _ = GT
    _ `compare` Pair _ _ = LT

    HighCard r1 rest1 `compare` HighCard r2 rest2 =
        (r1 `compare` r2) <> (compare `on` reverse) rest1 rest2

findHand :: [Card] -> Hand
findHand cards
    | allEqual suits && consecutive ranks =
        if lastRank == Ace then RoyalFlush else StraightFlush lastRank
    | allEqual suits = Flush lastRank
    | consecutive ranks = Straight lastRank
    | groups == [4, 1] = Quads firstRank lastRank
    | groups == [1, 4] = Quads lastRank firstRank
    | groups == [3, 2] = FullHouse firstRank lastRank
    | groups == [2, 3] = FullHouse lastRank firstRank
    | groups == [3, 1, 1] = ThreeOfAKind firstRank $ filter (/= firstRank) ranks
    | groups == [1, 3, 1] = ThreeOfAKind midRank $ filter (/= midRank) ranks
    | groups == [1, 1, 3] = ThreeOfAKind lastRank $ filter (/= lastRank) ranks
    | groups == [2, 2, 1] = TwoPairs midRank firstRank lastRank
    | groups == [2, 1, 2] = TwoPairs lastRank firstRank midRank
    | groups == [1, 2, 2] = TwoPairs lastRank midRank firstRank
    | groups == [2, 1, 1, 1] = Pair firstRank $ filter (/= firstRank) ranks
    | groups == [1, 2, 1, 1] = Pair midRank $ filter (/= midRank) ranks
    | groups == [1, 1, 2, 1] = Pair midRank $ filter (/= midRank) ranks
    | groups == [1, 1, 1, 2] = Pair lastRank $ filter (/= lastRank) ranks
    | otherwise = HighCard lastRank $ filter (/= lastRank) ranks
    where firstRank = head ranks
          midRank = ranks !! 2
          lastRank = last ranks
          groups = map length $ groupBy ((==) `on` rank) cards
          allEqual [] = False
          allEqual (x:xs) = all (==x) xs
          consecutive = (`isInfixOf` [Two .. Ace])
          suits = map suit cards
          ranks = map rank cards

lineToHands :: String -> (Hand, Hand)
lineToHands s = (f *** f) $ splitAt 5 cards
                where f = findHand . sort
                      cards = (mapMaybe stringToCard . words) s

solution54 :: IO ()
solution54 = do
    withFile "data/poker.txt" ReadMode $ \h -> do
        hands <- (map lineToHands . lines) <$> hGetContents h
        print $ length $ filter (uncurry (>)) hands
