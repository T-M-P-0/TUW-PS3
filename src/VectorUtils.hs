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

deleteChar :: Int -> Int -> Vec.Vector (Vec.Vector Char) -> Vec.Vector (Vec.Vector Char)
deleteChar x y buffer
  | x > 0 = buffer Vec.// [(y, newRow)]
  | y > 0 && x > 0 = buffer Vec.// [(y - 1, newRow)]
  | otherwise = buffer
  where
    row = buffer Vec.! y
    (left, right) = Vec.splitAt (x - 1) row
    newRow = left Vec.++ Vec.tail right