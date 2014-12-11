import Control.Monad (forM_)

import TicTacToe

printMoves :: State -> IO ()
printMoves st = forM_ (zip [0..] (moves st)) $ \(i, s) -> do
  print i
  pprint s
  putStrLn ""

main :: IO ()
main = do
    let st = posSeq [(1,1), (1,0), (0,2), (2,0)]
    pprint st
    print $ map abmax $ moves st
    printMoves st
