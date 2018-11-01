module Types.Rank where

data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving (Eq, Ord, Enum)

instance Show Rank where
    show Ace   = "A"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "T"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"

instance Read Rank where
  readsPrec _ (r:therest) =
    let r' = case r of
               'A' -> Ace
               '2' -> Two
               '3' -> Three
               '4' -> Four
               '5' -> Five
               '6' -> Six
               '7' -> Seven
               '8' -> Eight
               '9' -> Nine
               'T' -> Ten
               'J' -> Jack
               'Q' -> Queen
               'K' -> King
    in [(r', therest)]
  readsPrec _ _ = []