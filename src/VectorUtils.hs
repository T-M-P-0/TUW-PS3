module VectorUtils (deleteChar, insertChar) where

import Data.List.NonEmpty (append)
import qualified Data.Vector as Vec

insertChar :: Char -> Int -> Int -> Vec.Vector (Vec.Vector Char) -> Vec.Vector (Vec.Vector Char)
insertChar char row col buffer =
  buffer Vec.// [(row, updatedRow)] -- Update the row in the grid
  where
    currentRow = buffer Vec.! row
    (leftSideOfRow, rightSideOfRow) = Vec.splitAt col currentRow
    updatedRow = if col >= length currentRow then Vec.snoc currentRow char else leftSideOfRow Vec.++ Vec.singleton char Vec.++ rightSideOfRow

deleteChar :: Int -> Int -> Vec.Vector (Vec.Vector Char) -> (Int, Int, Vec.Vector (Vec.Vector Char))
deleteChar x y buffer
  | x > 0 = (x - 1, y, buffer Vec.// [(y, newRow)])
  | x == 0 && y > 0 = (length $ buffer Vec.! (y - 1), y - 1, left Vec.// [(Vec.length left - 1, Vec.last left Vec.++ row)] Vec.++ Vec.tail right)
  | otherwise = (x, y, buffer)
  where
    (left, right) = Vec.splitAt y buffer
    row = buffer Vec.! y
    (leftSideOfRow, rightSideOfRow) = Vec.splitAt (x - 1) row
    newRow = leftSideOfRow Vec.++ Vec.tail rightSideOfRow