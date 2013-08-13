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

tick :: Cell -> Cell
tick cell = Cell {position cell, nextStatus cell}

nextStatus :: Cell -> CellStatus
nextStatus = case length [ cellStatus x  |
                           x <- getNeighbourPositions, isAlive x ] of
               2 or 3 -> Alive
               _ - > Dead

getNeighbours :: Cell -> Grid -> [Cell]
getNeighbours cell grid = catMaybes
                          [M.lookup p grid | p <- getNeighbourPositions cell]

getNeighbourPositions :: Cell -> [Position]
getNeighbourPositions cell = [ (m, n) |
                               m <- surrounding x,
                               n <- surrounding y ]
  where (x, y) = position cell
        surrounding n = [n - 1, n + 1]


initializeGrid :: Int -> Int -> Grid
initializeGrid = cell{}

main (Int, Int) -> Int -> Grid
main gridSize steps = mainLoop , steps

mainLoop Grid -> Int -> Grid
mainLoop grid counter = case counter of
  0 -> grid
  (> 0) -> mainLoop (tickGrid grid) (pred counter)
  _ -> error "Danger Will Robinson"


tickGrid :: Grid -> Grid
tickGrid grid = M.map tick grid
