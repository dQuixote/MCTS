module Confrontation where

data Side = Light
          | Dark
          deriving (Eq)

data Piece = Aragorn
           | Boromir
           | Frodo
           | Gandalf
           | Gimli
           | Legolas
           | Merry
           | Pippin
           | Sam
           | Balrog
           | BlackRider
           | CaveTroll
           | FlyingNazgul
           | Orcs
           | Saruman
           | Shelob
           | Warg
           | WitchKing
           deriving (Eq, Show)

lightPieces :: [Piece]
lightPieces = [ Aragorn
              , Boromir
              , Frodo
              , Gandalf
              , Gimli
              , Legolas
              , Merry
              , Pippin
              , Sam
              ]

darkPieces :: [Piece]
darkPieces = [ Balrog
             , BlackRider
             , CaveTroll
             , FlyingNazgul
             , Orcs
             , Saruman
             , Shelob
             , Warg
             , WitchKing
             ]

strength :: Piece -> Int
strength p = case p of
    Aragorn      -> 4
    Boromir      -> 0
    Frodo        -> 1
    Gandalf      -> 5
    Gimli        -> 3
    Legolas      -> 3
    Merry        -> 2
    Pippin       -> 1
    Sam          -> 2
    Balrog       -> 5
    BlackRider   -> 3
    CaveTroll    -> 9
    FlyingNazgul -> 3
    Orcs         -> 2
    Saruman      -> 4
    Shelob       -> 5
    Warg         -> 2
    WitchKing    -> 5

side :: Piece -> Side
side p = if p `elem` lightPieces then Light else Dark

data LightCard = L1
               | L2
               | L3
               | L4
               | L5
               | Cloak
               | LMagic
               | Noble
               | LRetreat

instance Show LightCard where
    show card = case card of
        L1       -> "1"
        L2       -> "2"
        L3       -> "3"
        L4       -> "4"
        L5       -> "5"
        Cloak    -> "Elven Cloak"
        LMagic   -> "Magic"
        Noble    -> "Noble Sacrifice"
        LRetreat -> "Retreat"

lightCards :: [LightCard]
lightCards = [L1, L2, L3, L4, L5, Cloak, LMagic, Noble, LRetreat]

data DarkCard = D1
              | D2
              | D3
              | D4
              | D5
              | D6
              | Eye
              | DMagic
              | DRetreat

instance Show DarkCard where
    show card = case card of
        D1       -> "1"
        D2       -> "2"
        D3       -> "3"
        D4       -> "4"
        D5       -> "5"
        D6       -> "6"
        Eye      -> "Eye of Sauron"
        DMagic   -> "Magic"
        DRetreat -> "Retreat"

darkCards :: [DarkCard]
darkCards = [D1, D2, D3, D4, D5, D6, Eye, DMagic, DRetreat]

