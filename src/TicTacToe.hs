module TicTacToe where

import Data.Tree (Tree(..), unfoldTree)
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

-- | Examine the board and return whose turn it is, X or O
getTurn :: State -> Cell
getTurn st
  | k == 0    = XX
  | k == 1    = OO
  | otherwise = error "encountered invalid board state"
  where
    k = count XX st - count OO st
    count c = length . filter (== c) . cells

-- | A collection of cell extration functions
getRow :: State -> Int -> [Cell]
getRow s i = [ lookupCell s (i,j) | j <- [0,1,2] ]

getCol :: State -> Int -> [Cell]
getCol s i = [ lookupCell s (j,i) | j <- [0,1,2] ]

getDiag :: State -> [Cell]
getDiag s = [ lookupCell s (i,j) | (i,j) <- [(0,0), (1,1), (2,2)] ]

getAntiDiag :: State -> [Cell]
getAntiDiag s = [ lookupCell s (i,j) | (i,j) <- [(2,0), (1,1), (0,2)] ]

-- | Check if the given player has won the game
win :: Player -> State -> Bool
win player state = checkRows || checkCols || checkDiag || checkAntiDiag
  where
    c = piece player
    checkWin v = all (== v)
    checkRows = or [checkWin c $ getRow state i | i <- [0,1,2]]
    checkCols = or [checkWin c $ getCol state i | i <- [0,1,2]]
    checkDiag = checkWin c $ getDiag state
    checkAntiDiag = checkWin c $ getAntiDiag state

-- | Generate a list of new states reachable from the given state in one move
moves :: State -> [State]
moves st
  | win PlayerX st || win PlayerO st = []
  | otherwise = map (\p -> update st (p, getTurn st)) (freePos st)

-- | Generate a game tree from an initial game state
generate :: State -> Tree State
generate = unfoldTree (\s -> (s, moves s))

-- | A general purpose tree fold
foldTree :: (a -> b -> a) -> (a -> a -> a) -> a -> Tree b -> a
foldTree f _ a (Node b []) = f a b
foldTree f g a (Node b (t:rest)) = g (foldTree f g a t) (foldTree f g a (Node b rest))

-- | A general purpose tree pruner
prune :: Int -> Tree a -> Tree a
prune 0 t = Node (rootLabel t) []
prune n t = Node (rootLabel t) (map (prune (n-1)) (subForest t))

-- | Take a game state and return a rought valuation of it. Larger results
-- are better for X, smaller (esp. negative) results are better for O.
staticVal :: State -> Int
staticVal state
  | win PlayerX state = 1
  | win PlayerO state = -1
  | otherwise = 0

-- | Minimax valuation
maximizeTreeVal :: (Ord a) => Tree a -> a
maximizeTreeVal (Node x []) = x
maximizeTreeVal (Node _ subs) = maximum . map minimizeTreeVal $ subs

minimizeTreeVal :: (Ord a) => Tree a -> a
minimizeTreeVal (Node x []) = x
minimizeTreeVal (Node _ subs) = minimum . map maximizeTreeVal $ subs
