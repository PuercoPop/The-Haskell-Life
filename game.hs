module GameOfLife where

import qualified Data.Map as M
import Data.Maybe

data CellStatus = Alive | Dead

data Cell = Cell {position :: Position, cellStatus :: CellStatus}

type Position = (Int, Int)
type Grid = M.Map Position Cell

isAlive :: Cell -> Bool
isAlive cell = case cellStatus cell of
  Alive -> True
  _ -> False

isDead = not . isAlive

tick :: Cell -> Grid -> Cell
tick cell grid = Cell (position cell) (nextStatus cell grid)

nextStatus :: Cell -> Grid -> CellStatus
nextStatus cell grid = let len = length (getAliveNeighbours cell grid) in
  if len `elem` [2, 3] then Alive else Dead

getAliveNeighbours :: Cell -> Grid -> [CellStatus]
getAliveNeighbours cell grid  = [ cellStatus x  |  x <- getNeighbours cell grid, isAlive x ]

getNeighbours :: Cell -> Grid -> [Cell]
getNeighbours cell grid = catMaybes
                          [M.lookup p grid | p <- getNeighbourPositions cell]

getNeighbourPositions :: Cell -> [Position]
getNeighbourPositions cell = [ (m, n) |
                               m <- surrounding x,
                               n <- surrounding y ]
  where (x, y) = position cell
        surrounding n = [n - 1, n + 1]

initializeGrid :: Int -> Int -> [Position] -> Grid
initializeGrid height width alivecells =
  foldr insertDeadCell M.empty (combinations height width)
  where
    insertDeadCell :: Position -> Grid -> Grid
    insertDeadCell position m  = M.insert position (Cell position Dead) m

combinations :: Int -> Int -> [Position]
combinations height width = [ (x, y) | x <- [0 .. height],
                                       y <- [0 .. width]]

main :: (Int, Int) -> Int -> Grid
main gridSize steps = mainLoop (initializeGrid 20 20 [(0,0)]) steps

mainLoop :: Grid -> Int -> Grid
mainLoop grid 0 = grid
mainLoop grid counter | counter > 0 = mainLoop (tickGrid grid) (pred counter)
mainLoop _ _ = error "Danger Will Robinson"


tickGrid :: Grid -> Grid
tickGrid grid = M.map (flip tick grid) grid
