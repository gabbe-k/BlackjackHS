{- Blackjack (Lab2)
   Authors: Daniel Kvist, Daniel Perlkvist, Gabriel Käll
   Lab group: 69
 -}

module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- A1 ------------------------

-- An example Hand containing 2 cards
hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

-- Produces a list containing several identical elements
-- of the type Int, provided (length hand2) == 2
sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + (1 + size [])
            , 1 + (1 + 0)
            , 2
            ]


-- A2 ------------------------


-- Two predetermined cards used for aHand
aCard1 :: Card
aCard1 = Card Ace Spades

aCard2 :: Card
aCard2 = Card (Numeric 7) Clubs

-- A hand containing two predetermined cards
aHand :: Hand
aHand = [aCard1, aCard2]


-- Displays a given Card as a string reading
-- which card it is in common tongue
displayCard :: Card -> String
displayCard (Card (Numeric r) s)    = (show r) ++ " of " 
                                   ++ (show s)
displayCard (Card r s)              = "The " ++ (show r) 
                                   ++ " of " ++ (show s)


-- Displays a given hand as a string telling you
-- how many, and which cards are in the hand
display :: Hand -> String
display hand            =  "You have " ++ (show(size hand))
                        ++ " cards on hand:\n" 
                        ++ unlines [displayCard card | card <- hand]


-- A3 ------------------------

-- Assigns all card ranks a corresponding numeric value
valueRank :: Rank -> Int
valueRank (Numeric n)    = n
valueRank Ace            = 11
valueRank n              = 10

-- Counts how many aces are in a given hand
numberOfAces :: Hand -> Int
numberOfAces []                   = 0
numberOfAces ((Card Ace _):hand)  = 1 + (numberOfAces hand)
numberOfAces (card:hand)          = numberOfAces hand


-- Fetches the value of a card depending on its rank
valueCard :: Card -> Int
valueCard (Card r _)     = valueRank r

-- Calculates the total value of a given hand
value :: Hand -> Int
value []   = 0
value hand | ((sum [valueCard card | card <- hand]) > 21)
             && ((numberOfAces hand) > 0) 
             = (sum [valueCard card | card <- hand]) - 10*(numberOfAces hand)
           | otherwise = sum [valueCard card | card <- hand]



-- A4 ------------------------


-- Determines if the value of a hand exceeds 21
gameOver :: Hand -> Bool
gameOver hand = (value hand) > 21


-- Determines the winner of the round depending on the value of the hands,
-- with both hands being equal in value results in the bank winning
winner :: Hand -> Hand -> Player 
winner handG handB | not (gameOver(handG)) &&  
                     (value(handG) > value(handB)) = Guest
                   | not (gameOver(handG))
                     && (gameOver(handB))          = Guest
                   | otherwise                     = Bank

-- B1 ------------------------

-- A list of all ranks
allRanks :: [Rank]
allRanks  = [Numeric n | n <- [2..10]] ++ [Jack,Queen,King,Ace]

-- A list of all suits
allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

-- Builds full deck of cards using a list of ranks and a list of suits
fullDeck :: Deck
fullDeck = [Card rank suit | rank <- allRanks , suit <- allSuits]

prop_size_fullDeck = size fullDeck == 52

-- B2 ------------------------

-- Draws a card from the deck and adds to the hand of whomever drew
draw :: Deck -> Hand -> (Deck, Hand)
draw deck hand | deck == [] = error "draw: The deck is empty."
               | otherwise  = ((drop 1 deck), (hand ++ take 1 deck))

-- B3 ------------------------


-- Plays for the bank
playBank :: Deck -> Hand
playBank deck = playBank' (deck, [])

-- Does the work for playbank
-- Draws cards until the value of bankhand exceeds 16
playBank' :: (Deck, Hand) -> Hand
playBank' (d,h) | value (h) < 17 = playBank'(drawHand)
                | otherwise = h
    where 
        drawHand = draw d h

-- B4 ------------------------ 

-- Deletes the nth element in a deck
deleteN :: Int -> Deck -> Deck
deleteN i deck = xa ++ tail xb
    where 
    (xa,xb) = splitAt (i) deck

-- Shuffles a deck
shuffle :: [Double] -> Deck -> Deck
shuffle (x:xs) deck | deck == []     = deck
                    | otherwise      = (card) 
                    : shuffle xs (deleteN i deck)
    where 
    i = ceiling ((size deck) * x) - 1 -- Chosen index
    card = deck !! i --The randomly chosen card

-- B5 ------------------------ 
-- Properties that the func shuffle must follow

-- Helper function for prop_shuffle
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

-- Checks if there is exactly one of each card in the shuffled deck
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

-- Checks if the length of the deck is the same as before shuffling
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
                   length(deck) == length(shuffle randomlist deck)

-- B6 ------------------------

implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation