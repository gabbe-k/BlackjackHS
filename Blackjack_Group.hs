{- Blackjack (Lab2)
   Authors: Daniel Kvist, Daniel Perlkvist, Gabriel Käll
   Lab group: 69
 -}

module Blackjack where

import Cards
import RunGame


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
