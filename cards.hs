module Main where
  data Suit = Club | Diamond | Heart | Spade deriving (Show, Eq, Read)
  data Rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  type Card = (Suit, Rank)
  type Hand = [Card]

  
