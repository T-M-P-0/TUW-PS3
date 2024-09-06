module ExtensionHelper where

import qualified Data.Vector as Vec
import AppState (CursorPosition (..))

findMatchingBrace :: Vec.Vector (Vec.Vector Char) -> CursorPosition -> Maybe CursorPosition
findMatchingBrace buffer (CursorPosition x y) = do
  let line = buffer Vec.! y
  let char = line Vec.!? x
  case char of
    Just '{' -> findForward '{' '}' buffer (CursorPosition x y)
    Just '}' -> findBackward '{' '}' buffer (CursorPosition x y)
    Just '(' -> findForward '(' ')' buffer (CursorPosition x y)
    Just ')' -> findBackward '(' ')' buffer (CursorPosition x y)
    Just '[' -> findForward '[' ']' buffer (CursorPosition x y)
    Just ']' -> findBackward '[' ']' buffer (CursorPosition x y)
    _ -> Nothing

isBrace :: Char -> Bool
isBrace char
  | char == '{' = True
  | char == '}' = True
  | char == '(' = True
  | char == ')' = True
  | char == '[' = True
  | char == ']' = True
  | otherwise = False

findForward :: Char -> Char -> Vec.Vector (Vec.Vector Char) -> CursorPosition -> Maybe CursorPosition
findForward open close buffer (CursorPosition x y) = search (x + 1) y 0
  where
    search cx cy depth
      | cy >= Vec.length buffer = Nothing
      | otherwise =
          let row = buffer Vec.! cy
              char = row Vec.!? cx
              (newCx, newCy) = if cx + 1 >= Vec.length row then (0, cy + 1) else (cx + 1, cy)
           in case char of
                Just ch
                  | close == ch -> if depth == 0 then Just (CursorPosition cx cy) else search newCx newCy (depth - 1)
                  | open == ch -> search newCx newCy (depth + 1)
                  | otherwise -> search newCx newCy depth
                _ -> search newCx newCy depth

findBackward :: Char -> Char -> Vec.Vector (Vec.Vector Char) -> CursorPosition -> Maybe CursorPosition
findBackward open close buffer (CursorPosition x y) = search (x - 1) y 0
  where
    search cx cy depth
      | cy < 0  = Nothing
      | otherwise =
          let row = buffer Vec.! cy
              char = row Vec.!? cx
              (newCx, newCy) =
                if cx - 1 < 0
                  then
                    if cy - 1 >= 0 then (Vec.length (buffer Vec.! (cy - 1)) - 1, cy - 1) else (-1, -1)
                  else (cx -1, cy)
           in case char of
                Just ch
                  | ch == open -> if depth == 0 then Just (CursorPosition cx cy) else search newCx newCy (depth - 1)
                  | ch == close -> search newCx newCy (depth + 1)
                  | otherwise -> search newCx newCy depth
                _ -> search newCx newCy depth

findWordPositions :: Vec.Vector Char -> Vec.Vector (Vec.Vector Char) -> Vec.Vector (Vec.Vector CursorPosition)
findWordPositions word = Vec.imap (findWordInRow 0)
  where
    findWordInRow :: Int -> Int -> Vec.Vector Char -> Vec.Vector CursorPosition
    findWordInRow col rowIndex row
      | col >= Vec.length row = Vec.empty -- End of row, no more words to find
      | otherwise =
          let extractedWord = extractWordAt col row
              wordEndIndex = col + Vec.length extractedWord - 1
           in if extractedWord == word && isWordBoundary col wordEndIndex row
                then createPositions col wordEndIndex rowIndex Vec.++ findWordInRow (wordEndIndex + 1) rowIndex row
                else findWordInRow (col + 1) rowIndex row

isWordBoundary :: Int -> Int -> Vec.Vector Char -> Bool
isWordBoundary startIndex endIndex row =
  let isStartBoundary = startIndex == 0 || (row Vec.!? (startIndex - 1)) == Just ' '
      isEndBoundary = endIndex == Vec.length row - 1 || (row Vec.!? (endIndex + 1)) == Just ' '
   in isStartBoundary && isEndBoundary --

createPositions :: Int -> Int -> Int -> Vec.Vector CursorPosition
createPositions x1 x2 y = Vec.generate (x2 - x1 + 1) (\i -> CursorPosition (x1 + i) y)

extractWordAt :: Int -> Vec.Vector Char -> Vec.Vector Char
extractWordAt x row = word
  where
    (before, after) = Vec.splitAt x row
    findIndexUntilSpace word = Vec.length (Vec.takeWhile (\x -> x /= '(' || x /= ' ') word)

    startOffset = findIndexUntilSpace $ Vec.reverse before
    endOffset = findIndexUntilSpace after

    startIndex = max 0 (x - startOffset)
    endIndex = min (Vec.length row) (x + endOffset)
    word = Vec.slice startIndex (endIndex - startIndex) row