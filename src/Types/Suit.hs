module Types.Suit where

import System.Console.ANSI ( Color(..) )

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq, Enum)

instance Show Suit where
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"
  show Spades   = "♠"

instance Read Suit where
  readsPrec _ (s:therest) =
    let s' = case s of
               'c' -> Clubs
               'd' -> Diamonds
               'h' -> Hearts
               's' -> Spades
    in [(s', therest)]
  readsPrec _ _ = []

getSuitColor :: Suit -> System.Console.ANSI.Color
getSuitColor Clubs    = Green
getSuitColor Diamonds = Cyan
getSuitColor Hearts   = Red
getSuitColor Spades   = White