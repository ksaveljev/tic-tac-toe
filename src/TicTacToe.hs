module TicTacToe where

import Data.Array.IArray (Array, listArray, indices, elems, (!), (//))

data Cell = Blank | XX | OO deriving (Enum, Read, Eq, Ord)

instance Show Cell where
    show Blank = "."
    show XX    = "X"
    show OO    = "O"

data Player = PlayerX | PlayerO deriving (Show, Eq)

type Pos = (Int, Int)

type State = Array Pos Cell

boardSize :: Int
boardSize = 9

boardInds :: ((Int, Int), (Int, Int))
boardInds = ((0,0), (2,2))

-- | Return the "opposite" cell contents
opp :: Cell -> Cell
opp Blank = Blank
opp XX    = OO
opp OO    = XX

-- | Return the piece assigned to each player
piece :: Player -> Cell
piece PlayerX = XX
piece PlayerO = OO

-- | Empty board
empty :: State
empty = listArray boardInds $ replicate boardSize Blank

-- | Return a list of board positions in order
positions :: [Pos]
positions = indices empty

-- | Return a list of cells in index order
cells :: State -> [Cell]
cells = elems

-- | Lookup the cell at a given position
lookupCell :: State -> Pos -> Cell
lookupCell = (!)

-- | Add a new move to the board (order of args is fold left folding)
update :: State -> (Pos, Cell) -> State
update st pc = st // [pc]

-- | Return the list of unoccupied positions on the board
freePos :: State -> [Pos]
freePos st = filter (\p -> st ! p == Blank) positions
