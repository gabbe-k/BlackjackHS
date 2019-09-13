module Blackjack where

import Cards
import RunGame
import System.IO
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

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 2 + size []
            , 2
            ]

--Task A2

displayCard :: Card -> String
displayCard (Card (Numeric r) s) = show (r) ++ " of " ++ show(s) 
displayCard (Card r s)           = show (r) ++ " of " ++ show(s) 

display :: Hand -> String
display hand = "Your hand: \n" ++  unlines [ " >  " ++displayCard card | card <- hand] ++ "\n"

main = putStr (display hand2)  

--Task A3
valueRank :: Rank -> Int
valueRank (Numeric r) = r
valueRank r | r == Ace = 11
            | otherwise = 10

valueCard :: Card -> Int
valueCard (Card r _) = valueRank r

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (card:hand) | rank card == Ace = 1 + numberOfAces(hand)
                         | otherwise = numberOfAces(hand)

value :: Hand -> Int
value [] = 0
value (card:hand) | valueCard(card) + value(hand) < 21 = valueCard(card) + value(hand)
                  | otherwise = valueCard(card) + value(hand) - (10 * numberOfAces(card:hand)) 

--Task A4

gameOver :: Hand -> Bool
gameOver hand | value(hand) > 21 = True
              | otherwise = False

highestValHand :: Hand -> Hand -> Player
highestValHand guestHand bankHand | value(guestHand) > value(bankHand) = Guest
                                  | otherwise = Bank


winner :: Hand -> Hand -> Player 
winner guestHand bankHand | value(guestHand) == value(bankHand) = Bank
                          | (value(guestHand) > 21) && (value(bankHand) > 21)  = Bank
                          | (value(guestHand)<= 21)  || (value(bankHand) <= 21) = highestValHand guestHand bankHand