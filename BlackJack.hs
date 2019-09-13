module Blackjack where

import Cards
import RunGame
import System.IO
import Test.QuickCheck hiding (shuffle)

--Task A1

hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 2 + size []
            , 2
            ]

--Useful definitions

aCard1 :: Card 
aCard1 = Card (Numeric 6) Diamonds

aCard2 :: Card
aCard2 = Card King Spades

aHand :: Hand 
aHand = [aCard1, aCard2]

--Task A2

displayCard :: Card -> String
displayCard (Card (Numeric r) s) = show (r) ++ " of " ++ show(s) 
displayCard (Card r s) = show (r) ++ " of " ++ show(s) 

display :: Hand -> String
--display hand = unlines [displayCard card | card <- hand]
display card = displayCard(card)
display (card:hand) = displayCard(card) (++) display(hand)

main = putStrLn (displayCard aCard1)  

--display :: Hand -> String
--display aHand =  