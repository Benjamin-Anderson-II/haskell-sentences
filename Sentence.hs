-- Benjamin Anderson II 934-353-159
-- Due Data: May 2, 2023

-- This code is for HW3 from CS_381 @ Oregon State University 2023
-- The intention of this assignment is to practice syntax

-- Homework 3 template
module Sentence where

-- Grammar for the animal sentence language:
--
--   <sentence> ->  <noun> <verb> [<noun>]  
--                |  <sentence> `and` <sentence>
--          
--   <noun>   -> <adj> <noun> | <noun> `and` <noun>
--          | `cats` | `dogs` | `bears` | `goats`

--   <verb>   ->  `chase` | `cuddle` | `hug` | `scare`
--   <adj>    ->  `sad` | `small` | `big` | `happy`

data Sentence
   = NVN Noun Verb Noun
   | NV Noun Verb
   | And Sentence Sentence
   | End
  deriving (Eq,Show)

data Adj = Sad | Small | Big | Happy-- finish adjectives
  deriving (Eq,Show)

data Noun 
  = NP Adj Noun
  | NAnd Noun Noun
  | Bears | Cats | Dogs | Goats
  deriving (Eq,Show)

data Verb = Chase | Cuddle | Hug | Scare
  deriving (Eq,Show)



ex1 :: Sentence
ex1 = NVN Cats Hug Dogs

ex2 :: Sentence
ex2 = NVN (NP Small Cats) Hug Dogs

ex3 :: Sentence
ex3 = NVN (NAnd Dogs Cats) Chase Goats

ex4 :: Sentence
ex4 = NVN (NAnd (NP Happy Dogs) Cats) Chase Goats


-- | Build a sentence from a noun verb noun.
-- | buildS2 Cats Hug Cats
-- | NVN Cats Hug Cats

buildS2 :: Noun -> Verb ->Noun-> Sentence
buildS2 n1 v n2 = NVN n1 v n2

-- | Build a sentence from a noun verb 
-- | buildS1 Cats Hug 
-- | NV Cats Hug 

buildS1 :: Noun -> Verb ->Sentence
buildS1 n v = NV n v


-- | Build a noun phrase from an adjective and noun
-- | buildNP Happy Dogs
-- | NP Happy Dogs

buildNP :: Adj -> Noun -> Noun
buildNP a n = NP a n

-- | Build a noun conjunction from two nouns
-- | buildNAnd Dogs Cats
-- | NAnd Dogs Cats

buildNAnd :: Noun -> Noun -> Noun
buildNAnd n m =
  case (n,m) of
    (n,m) -> NAnd n m

-- | Build a sentence that is a conjunction of a list of other sentences.
-- | conjunction [ex1, ex2]
-- | And (NVN Cats Hug Dogs) (NVN (NP Small Cats) Hug Dogs)
-- | The End is used if no sentences are given
 
conjunction :: [Sentence] -> Sentence
conjunction l =
  case l of
    []     -> End
    (s:ss) -> if (length ss == 0)
                then s
                else And (s) (conjunction ss)

-- | Pretty print a sentence.
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (And l r)   = pretty l ++ " and " ++ pretty r
pretty (NV s v)     = prettyNoun s ++ " " ++ prettyVerb v
pretty (End) = "."

-- | Pretty print a noun.
prettyNoun :: Noun -> String
prettyNoun n =
  case n of
    Cats  -> "cats"
    Dogs  -> "dogs"
    Bears -> "bears"
    Goats -> "goats"
    (NP a n) -> prettyAdj a ++ " " ++ prettyNoun n
    (NAnd m n) -> prettyNoun m ++ " and " ++ prettyNoun n

-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb v = 
  case v of
    Chase  -> "chase"
    Cuddle -> "cuddle"
    Hug    -> "hug"
    Scare  -> "scare"

-- | Pretty print an adjective.
prettyAdj :: Adj -> String
prettyAdj a =
  case a of
    Sad   -> "sad"
    Small -> "small"
    Big   -> "big"
    Happy -> "happy"

-- | Does the sentence contain only chase and scare?
-- | isMean ex2 => False
-- | isMean ex3 => True
isMean :: Sentence -> Bool
isMean s =
  case s of
    (And l r) -> isMean l && isMean r
    (NVN _ Chase _)  -> True
    (NVN _ Cuddle _) -> False
    (NVN _ Hug _)    -> False
    (NVN _ Scare _)  -> True
    (NV _ Chase)     -> True
    (NV _ Cuddle) -> False
    (NV _ Hug)    -> False
    (NV _ Scare)  -> True
    End -> False

nounCount :: Noun -> Int
nounCount n =
  case n of
    Cats  -> 1 
    Dogs  -> 1
    Bears -> 1
    Goats -> 1
    (NP _ n)   -> 1 + nounCount n
    (NAnd m n) -> nounCount n + nounCount m + 1

-- |Count the number of words in a sentence
-- | wordCount ex4
--    6

wordCount :: Sentence -> Int
wordCount s =
  case s of
    (And l r) ->   wordCount l + wordCount r + 1
    (NVN s v o) -> nounCount s + 1 + nounCount o
    (NV s v) ->    nounCount s + 1
    End ->         0
