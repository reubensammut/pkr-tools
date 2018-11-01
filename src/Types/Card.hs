module Types.Card where

import Types.Rank
import Types.Suit

import System.Console.ANSI (setSGRCode
                           , Color(..)
                           , ColorIntensity(..)
                           , ConsoleLayer(..)
                           , SGR(..) )

data Card = Card Rank Suit
  deriving Eq

getCardColorPrefix :: Suit -> String
getCardColorPrefix x = setSGRCode [SetColor Foreground Dull (getSuitColor x)]

instance Show Card where
  show (Card r s) = getCardColorPrefix s ++ (show r) ++ show(s) ++ setSGRCode [Reset]

instance Read Card where
  readsPrec _ (r:s:therest) =
    let r' = read [r] :: Rank
        s' = read [s] :: Suit
    in [(Card r' s', therest)]
  readsPrec _ _ = []
