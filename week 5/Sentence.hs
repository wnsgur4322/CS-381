module Sentence where

-- Grammar for the animal sentence language:
--
-- s ::= n v n | s `and` s
-- n ::= `cats` | `dogs` | `ducks`
-- v ::= `chase` | `cuddle`

data Sentence
  = NVN Noun Verb Noun
  | And Sentence Sentence
  deriving (Eq, Show)

data Noun
  = Cats
  | Dogs
  | Ducks
  deriving (Eq, Show)

data Verb
  = Chase
  | Cuddle
  deriving (Eq, Show)


-- | The sentence: cats chase dogs and dogs cuddle ducks
ex1 :: Sentence
ex1 = And (NVN Cats Chase Dogs) (NVN Dogs Cuddle Ducks)

-- | Does the sentence contain only cuddling?
isNice :: Sentence -> Bool
-- step 2, seperate in 2 cases.
isNice (NVN n1 v n2) = v == Cuddle
isNice (And s1 s2) = isNice s1 && isNice s2


-- in Homework 3, you should convert grammar to Haskell like above example

