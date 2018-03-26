data Color = Red | Black
            deriving (Eq,Show)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq,Show)

data Move = Draw | Discard Card
            deriving (Eq,Show)

data Rank = Num Int | Jack | Queen | King | Ace
            deriving (Eq,Show)

data Card = Card { suit :: Suit, rank :: Rank }
            deriving (Eq,Show)


cardColor :: Card -> Color
cardColor card = if suit card == Spades || suit card == Clubs then Black else Red

main :: IO ()
main = return ()
