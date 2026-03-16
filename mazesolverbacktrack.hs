import System.IO

type Maze = [[Int]]
type Pos  = (Int, Int)

valid :: Maze -> Pos -> Bool
valid maze (x,y) =
  x >= 0 && y >= 0 &&
  x < length maze &&
  y < length (head maze) &&
  (maze !! x !! y == 0)

solve :: Maze -> Pos -> Pos -> [Pos] -> Maybe [Pos]
solve maze start end path
  | start == end = Just (reverse (start:path))
  | otherwise =
      let (x,y) = start
          moves = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
          try [] = Nothing
          try (m:ms)
            | valid maze m && notElem m path =
                case solve maze m end (start:path) of
                  Just p  -> Just p
                  Nothing -> try ms
            | otherwise = try ms
      in try moves

readRow :: Int -> Int -> IO [Int]
readRow rowNum size = do
  putStrLn $ "Enter row " ++ show rowNum ++ " (" ++ show size ++ " numbers, 0=open, 1=wall):"
  line <- getLine
  let nums = map read (words line)
  if length nums /= size
    then do
      putStrLn $ "Invalid row length! Please enter exactly " ++ show size ++ " numbers."
      readRow rowNum size
    else return nums

readMaze :: Int -> IO Maze
readMaze size = sequence [readRow i size | i <- [1..size]]

main :: IO ()
main = do
  putStrLn "Enter maze size (e.g., 5 for 5x5):"
  sizeStr <- getLine
  let size = read sizeStr :: Int
  putStrLn $ "Enter a " ++ show size ++ "x" ++ show size ++ " maze (0=open, 1=wall):"
  maze <- readMaze size
  let start = (0,0)
      end   = (size-1,size-1)
  case solve maze start end [] of
    Nothing   -> putStrLn "No path found!"
    Just path -> do
      putStrLn "\nSolved Path:"
      print path
