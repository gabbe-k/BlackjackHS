module Blackjack where

import Cards
import RunGame
import System.IO
import Test.QuickCheck hiding (shuffle)

--Task A1

hand2 = [Card (Numeric 2) Hearts, Card Jack Spades, Card King Diamonds, Card Queen Spades]

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
displayCard (Card r s)           = show (r) ++ " of " ++ show(s) 

display :: Hand -> String
display hand = "Your hand: \n" ++  unlines [ " >  " ++displayCard card | card <- hand] ++ "\n"

{-display :: Hand -> String
display []           = []
display (card:hand) | length hand == 0 = (displayCard(card)) ++  display(hand) 
                    | otherwise = (displayCard(card)) ++ ", " ++  display(hand) 
-}
{-
display :: Hand -> String
display [] _          = []
display (card:hand) (handLen) | length length  == (length (hand) + 1) = "Your current hand: "
                                  | length hand == 0 = (displayCard(card)) ++  display(hand) 
                                  | otherwise = "Len cardhand " ++ show(length (card:hand)) ++ " " ++ "Len hand " ++ show(length(hand)) ++ " ||| " ++ (displayCard(card)) ++ ", " ++  display(hand) 
    where handLen = length(hand)
-}

main = putStr (display hand2)  

--Task A3
valueRank :: Rank -> Int
valueRank (Card (Numeric n))
valueRank (Card r)

valueCard :: Card -> Int

numberOfAces :: Hand -> Int

value :: Hand -> Int

--display :: Hand -> String
--display aHand =  