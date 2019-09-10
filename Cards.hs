#OPTIONS_GHC -Wincomplete-patterns

data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)
            
data Rank = Numeric Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

data Card = Card Rank Suit 
            deriving (Eq, Show)

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Rank 
rank (Card s _) = s

type Hand = [Card]
type Deck = [Card]

size :: Num a => Hand -> a 
size [] = 0
size (card:hand) = 1 + size hand

hand2 :: Hand 
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

