module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

--Useful definitions

aCard1 :: Card 
aCard1 = Card (Numeric 6) Diamonds

aCard2 :: Card
aCard2 = Card King Spades

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
allRanks :: [Rank]
allRanks  = [Numeric n | n <- [2..10]] ++ [Jack,Queen,King,Ace]

allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

fullDeck :: Deck
fullDeck = [Card rank suit | rank <- allRanks , suit <- allSuits]

prop_size_fullDeck = size fullDeck == 52

--B2
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty"
draw deck hand = (drop 1 deck, (take 1 deck) ++ hand)

--B3
playBank :: Deck -> Hand