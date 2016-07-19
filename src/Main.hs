module Main where

import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable
import System.IO

data Side = X | O deriving Eq
data State = Empty | Filled Side
data Row a = Row { left :: a, center :: a, right :: a }
data Board a = Board { top :: a, middle :: a, bottom :: a }

instance Foldable Row where foldMap f (Row x y z) = f x <> f y <> f z
instance Foldable Board where foldMap f (Board x y z) = f x <> f y <> f z

instance Functor Row where fmap f (Row x y z) = Row (f x) (f y) (f z)
instance Functor Board where fmap f (Board x y z) = Board (f x) (f y) (f z)

instance Traversable Row where traverse f (Row x y z) = Row <$> f x <*> f y <*> f z
instance Traversable Board where traverse f (Board x y z) = Board <$> f x <*> f y <*> f z

emptyR = Row Empty Empty Empty
emptyBoard = Board emptyR emptyR emptyR

showSide X = 'X'
showSide O = 'O'

showState (Filled s) = showSide s
showState Empty = '.'

putRow row = putChar ' '
          >> putChar (left row)
          >> putStr " | "
          >> putChar (center row)
          >> putStr " | "
          >> putChar (right row)
          >> putStrLn " "

putBoard board = putStrLn "   |   |   "
              >> putRow (top board)
              >> putStrLn "---|---|---"
              >> putRow (middle board)
              >> putStrLn "---|---|---"
              >> putRow (bottom board)
              >> putStrLn "   |   |   "

ixmap f = snd . (mapAccumL.mapAccumL) (\i s -> (i+1, f i s)) 1

numbered = ixmap number
  where number i s = case s of Empty -> head (show i) ; _ -> showState s

place pos side = ixmap replace
  where replace i s0 = if i==pos then (Filled side) else s0

legalMove pos = all and . ixmap legal
  where legal i s = i /= pos || case s of Empty -> True ; _ -> False

gameOver :: Board (Row State) -> Bool
gameOver = all.all $ \s -> case s of (Filled _) -> True ; Empty -> False

runs :: Monoid m => [(State -> m) -> Board (Row State) -> m]
runs = [(.top) . foldMap, (.middle) .  foldMap, (.bottom) .  foldMap
       ,foldMap . (.left), foldMap . (.center), foldMap . (.right)
       ,((.(ap [left.top,center.middle,right.bottom] . pure)) . foldMap)
       ,((.(ap [right.top,center.middle,left.bottom] . pure)) . foldMap)
       ]

won side board = any win runs where
  win run = getAll $
    run (All.(\s->case s of Filled x -> x == side ; _ -> False)) board

other X = O
other O = X

data Move = Position Int | Quit

getMove side board = do
  putStr $ showSide side : " moves: "
  hFlush stdout
  line <- getLine
  if line == "q" then return Quit
  else case reads line of
    [(pos, [])] ->
      if pos < 1 || pos > 9 then do
        putStrLn "Invalid input!"
        getMove side board
      else if not (legalMove pos board) then do
        putStrLn $ "Illegal move!"
        getMove side board
      else return (Position pos)
    _ -> do
      putStrLn "Invalid input!"
      getMove side board

(&) = flip ($)

game side board = do
  putBoard (numbered board)
  move <- getMove side board
  case move of
    Quit -> putStrLn "Goodbye!"
    Position pos -> place pos side board & \board -> do
      if won side board then do
        putBoard $ (fmap.fmap) showState board
        putStrLn $ showSide side : " wins!"
      else if gameOver board then do
        putBoard $ (fmap.fmap) showState board
        putStrLn "Game over"
      else game (other side) board

main :: IO ()
main = game X emptyBoard
