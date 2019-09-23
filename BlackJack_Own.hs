{- Blackjack (Lab2)
   Authors: Daniel Kvist, Daniel Perlkvist, Gabriel Käll
   Lab group: 69
 -}

module Blackjack where


import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import Data.List

--Useful definitions

aCard1 :: Card 
aCard1 = Card (Numeric 6) Diamonds

aCard2 :: Card
aCard2 = Card (Numeric 6) Spades

aCard3 :: Card
aCard3 = Card Ace Spades

aHand :: Hand 
aHand = [aCard1, aCard2]

aHand2 = [Card (Numeric 8) Hearts, Card Ace Diamonds]

--Task A1
--Example showing how a recursive function would process a list

sizeSteps :: [Int]
sizeSteps = [ size aHand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + (1 + size [])
            , 1 + (1 + 0)
            , 2
            ]

--Task A2
--Functions used for displaying cards and hands neatly for the user

--Displays a card in the format "x of y"
displayCard :: Card -> String
displayCard (Card (Numeric r) s) = show (r) ++ " of " ++ show(s) 
displayCard (Card r s)           = show (r) ++ " of " ++ show(s) 

--Displays all cards in a hand using previous function and list comprehension
display :: Hand -> String
display hand = "Your hand: \n" ++ unlines [ " >  " ++displayCard card | card <- hand]

--Example: putStr is necessary to aquire the right newline formatting
displayExamp = putStr (display aHand2)  

--Task A3
--Functions counting the points given to the player for having cards/hands

--Returns points associated with certain ranks
valueRank :: Rank -> Int
valueRank (Numeric r)   = r
--Aces are worth one if sum of hand exceeds 21
valueRank Ace           = 11 
valueRank r             = 10

--Counts the number of aces in a hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (card:hand) | rank card == Ace = 1 + numberOfAces(hand)
                         | otherwise = numberOfAces(hand)

--Counts the total value of a players hand
--Deletes 10 points for each ace if value > 21

value :: Hand -> Int
value [] = 0
value (card:hand) | valueRank(rank(card)) + value(hand) > 21 
                   && numberOfAces(card:hand) > 0
                   = valueRank(rank(card)) + value(hand) 
                   - (10 * numberOfAces(card:hand)) 
                  | otherwise 
                   = valueRank(rank(card)) + value(hand) 

-- Calculates the total value of a given hand

{-
-- Fetches the value of a card depending on its rank
valueCard :: Card -> Int
valueCard (Card r _)     = valueRank r

value :: Hand -> Int
value []   = 0
value hand | ((sum [valueRank(rank(card)) card | card <- hand]) > 21)
             && ((numberOfAces hand) > 0) 
             = (sum [valueRank(rank(card)) card | card <- hand]) - 10*(numberOfAces hand)
           | otherwise = sum [valueRank(rank(card)) card | card <- hand]    -}               
 
    
--Task A4
--Functions used for checking various win/lose conditions

--Checks if a player is bust
gameOver :: Hand -> Bool
gameOver hand = value(hand) > 21 

--Checks which player has won depending on boths hands
winner :: Hand -> Hand -> Player 
winner handG handB | not (gameOver(handG)) &&  
                     (value(handG) > value(handB)) = Guest
                   | not (gameOver(handG))
                     && (gameOver(handB))          = Guest
                   | otherwise                     = Bank

--B1

--A list of all ranks
allRanks :: [Rank]
allRanks  = [Numeric n | n <- [2..10]] ++ [Jack,Queen,King,Ace]

--A list of all suits
allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

-- Builds full deck of cards using a list of ranks and a list of suits
fullDeck :: Deck
fullDeck = [Card rank suit | rank <- allRanks , suit <- allSuits]

prop_size_fullDeck = size fullDeck == 52

--B2

-- Draws a card from the deck and adds to the hand of whomever drew
draw :: Deck -> Hand -> (Deck, Hand)
draw deck hand | deck == [] = error "draw: The deck is empty."
               | otherwise  = ((drop 1 deck), (hand ++ take 1 deck))

--B3

--Plays for the bank
playBank :: Deck -> Hand
playBank deck = playBank' (deck, [])

--Does the work for playbank
--Draws cards until the value of bankhand exceeds 16
playBank' :: (Deck, Hand) -> Hand
playBank' deckBankH | value(snd(drawHand)) < 17 = playBank'(drawHand)
                    | otherwise = h
    where d = fst(deckBankH) 
          h = snd(deckBankH)
          drawHand = draw d h

--B4 

--Deletes the nth element in a deck
deleteN :: Int -> Deck -> Deck
deleteN i deck = xa ++ tail xb
    where 
    xa = fst(splitAt (i) deck) --Beginning of deck 
    xb = snd(splitAt (i) deck) --element i + Rest of deck 

--Shuffles a deck
shuffle :: [Double] -> Deck -> Deck
shuffle randList deck | deck == []     = deck
                      | otherwise      = (card) : 
                        shuffle (tail randList) (deleteN i deck)
    where 
    i = ceiling ((size deck) * (head randList)) - 1 -- Chosen index
    card = deck !! i --The randomly chosen card

--Task B5 
--Properties that the func shuffle must follow

--Helper function for prop_shuffle
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

--Checks if there is exactly one of each card in the shuffled deck
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

--Checks if the length of the deck is the same as before shuffling
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
                   length(deck) == length(shuffle randomlist deck)

--B6

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

