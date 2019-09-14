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

hand2 = [Card (Numeric 2) Hearts, Card Jack Spades, Card Ace Diamonds, Card Ace Spades]

--Task A1
--Example showing how a recursive function would process a list

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 2 + size []
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
display hand = "Your hand: \n" ++  unlines [ " >  " ++displayCard card | card <- hand]

--putStrLn is necessary to aquire the right formatting (converts \n to newlines)
main = putStrLn (display hand2)  

--Task A3
--Functions for counting the points given to the player for having cards/hands

--Returns points associated with certain ranks
valueRank :: Rank -> Int
valueRank (Numeric r) = r
valueRank r | r == Ace = 11
            | otherwise = 10

--Helper function for the previous function
valueCard :: Card -> Int
valueCard (Card r _) = valueRank r

--Counts the number of aces in a hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (card:hand) | rank card == Ace = 1 + numberOfAces(hand)
                         | otherwise = numberOfAces(hand)

--Counts the total value of a players hand, and deletes points if value > 21
value :: Hand -> Int
value [] = 0
value (card:hand) | valueCard(card) + value(hand) <= 21 = valueCard(card) + value(hand)
                  | otherwise = valueCard(card) + value(hand) - (10 * numberOfAces(card:hand)) 

--Task A4
--Functions used for checking various win/lose conditions

--Checks if a player is bust
gameOver :: Hand -> Bool
gameOver hand | value(hand) > 21 = True
              | otherwise = False

--Compares 2 hands and returns the player with the highest
highestValHand :: Hand -> Hand -> Player
highestValHand guestHand bankHand | value(guestHand) > value(bankHand) = Guest
                                  | otherwise = Bank

--Checks which player has won depending on boths hands
winner :: Hand -> Hand -> Player 
winner guestHand bankHand | value(guestHand) == value(bankHand) = Bank
                          | (value(guestHand) > 21) && (value(bankHand) > 21)  = Bank
                          | (value(guestHand)<= 21)  || (value(bankHand) <= 21) = highestValHand guestHand bankHand