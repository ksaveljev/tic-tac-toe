module TicTacToe where

import Data.List (foldl', intercalate)
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

-- | Generate a State given a function from positions to cells
genState :: (Pos -> Cell) -> State
genState f = foldl' (\s p -> update s (p, f p)) empty positions

-- | Assumes that X goes first, constructs a game state from a sequence of
-- positions
posSeq :: [Pos] -> State
posSeq ps = foldl' update empty (zip ps (cycle [XX, OO]))

-- | Return a multi-line string representation of the state
pretty :: State -> String
pretty st = let chars = prettyOneLine st
                rows = [ take 3 (drop (3 * i) chars) | i <- [0..2] ]
            in intercalate "\n" rows

pprint :: State -> IO ()
pprint = putStrLn . pretty

-- | Return a one-line string representation of the state
prettyOneLine :: State -> String
prettyOneLine = concatMap show . cells
