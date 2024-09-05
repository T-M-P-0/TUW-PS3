module Editor (start) where

import AppState (AppState (..), CursorPosition (..), Mode (..), Window (..))
import Command (Command (..))
import CommandHandler (commandHandler)
import CommandParser (parse)
import qualified Data.Vector as Vec
import Graphics.Vty (Cursor (Cursor))
import qualified Graphics.Vty as Vty
import InputReader (readInput)
import KeyInfo (KeyInfo (..))
import Margin (Margin (..))

start :: IO ()
start = do
  vty <- Vty.mkVty Vty.defaultConfig
  window <- getWindowConfig vty
  mainLoop (AppState {buffer = Vec.fromList [Vec.fromList ""], cursorPosition = CursorPosition {x = 0, y = 0}, window = window, mode = EditFile, filePath = "", previousBuffer = Vec.empty, errors = [], exit = False}) vty

mainLoop :: AppState -> Vty.Vty -> IO ()
mainLoop state@(AppState _ (CursorPosition x y) _ _ _ _ _ True) vty = do
  -- Ask for exit if the file is not saved.
  Vty.shutdown vty
  putStrLn $ "Exit editor" ++ show state
mainLoop state vty = do
  newState <- handleUpdate state vty
  mainLoop newState vty

handleUpdate :: AppState -> Vty.Vty -> IO AppState
handleUpdate state@(AppState buffer cursorPosition window mode filePath previousBuffer errors False) vty = do
  updateDisplay vty state
  maybeKeyInfo <- readInput vty
  let command = parse maybeKeyInfo mode
  case command of
    Just _ -> do
      currWindowSize <- getWindowConfig vty
      commandHandler command (AppState buffer cursorPosition currWindowSize mode filePath previousBuffer errors False)
    Nothing ->
      return state

updateDisplay :: Vty.Vty -> AppState -> IO ()
updateDisplay vty (AppState buffer cursorPos@(CursorPosition x y) (Window width height) mode filePath previousBuffer errors exit) = do
  case mode of
    ReadFile -> do
      let img = createImageForReadFileMode buffer readFileModeImageTitle readFileModeImageText
      let (Margin left right top bottom) = marginConfigReadFileMode
      Vty.update vty (Vty.picForImage img) {Vty.picCursor = Vty.Cursor (x + left) (y + top)}
    WriteFile -> do
      let img = createImageForReadFileMode buffer writeFileModeImageTitle writeFileModeImageText
      let (Margin left right top bottom) = marginConfigReadFileMode
      Vty.update vty (Vty.picForImage img) {Vty.picCursor = Vty.Cursor (x + left) (y + top)}
    EditFile -> do
      let img = highlight cursorPos buffer
      let (Margin left right top bottom) = marginConfigEditMode
      Vty.update vty (Vty.picForImage img) {Vty.picCursor = Vty.Cursor (x) y}

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

findForward :: Char -> Char -> Vec.Vector (Vec.Vector Char) -> CursorPosition -> Maybe CursorPosition
findForward open close buffer (CursorPosition x y) = search (x + 1) y 0
  where
    search cx cy openBraceCount
      | cy >= Vec.length buffer = Nothing
      | otherwise =
          let row = buffer Vec.! cy
              char = row Vec.! cx
              newCy = if cx + 1 >= Vec.length row then cy + 1 else cy
           in case char of
                _
                  | close == char -> if openBraceCount == 0 then Just (CursorPosition cx cy) else search (cx + 1) newCy (openBraceCount - 1)
                  | open == char -> search (cx + 1) newCy (openBraceCount + 1)
                  | otherwise -> search (cx + 1) newCy openBraceCount

findBackward :: Char -> Char -> Vec.Vector (Vec.Vector Char) -> CursorPosition -> Maybe CursorPosition
findBackward open close buffer (CursorPosition x y) = search x y 0
  where
    search cx cy depth
      | cy < 0 = Nothing
      | cx < 0 = search (Vec.length (buffer Vec.! (cy - 1)) - 1) (cy - 1) depth
      | otherwise =
          let line = buffer Vec.! cy
              char = line Vec.! cx
           in if char == close
                then search (cx - 1) cy (depth + 1)
                else
                  if char == open
                    then if depth == 0 then Just (CursorPosition cx cy) else search (cx - 1) cy (depth - 1)
                    else search (cx - 1) cy depth

highlight :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> Vty.Image
highlight cursorPos@(CursorPosition x y) buffer = if x < Vec.length currRow && currChar /= ' ' then highlightWordPositions cursorPos buffer else createImageText buffer
  where
    currRow = buffer Vec.! y
    currChar =  currRow Vec.! x 

highlightWordPositions :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> Vty.Image
highlightWordPositions (CursorPosition cx cy) buffer = if multipleWordMatches then highlightedImage else createImageText buffer 
  where
    currentRow = buffer Vec.! cy
    currChar =  currentRow Vec.! cx 
    currentWord = extractWordAt cx currentRow
    positionsToHighlight = Vec.concat $ Vec.toList (findWordPositions currentWord buffer)
    highlightedImage = Vty.vertCat $ Vec.toList $ Vec.imap (\index row -> rowToImage index row positionsToHighlight) buffer
    multipleWordMatches = Vec.length positionsToHighlight > length currentWord
    highlightImage = cx < Vec.length currentRow && currChar /= ' ' && multipleWordMatches
  
rowToImage :: Int -> Vec.Vector Char -> Vec.Vector CursorPosition -> Vty.Image
rowToImage rowIndex row positionsToHighlight = Vty.horizCat $ Vec.toList $ Vec.imap charToImageRow row
  where
    charToImageRow :: Int -> Char -> Vty.Image
    charToImageRow colIndex char =  charToImage char (isHighlighted colIndex)
    isHighlighted :: Int -> Bool
    isHighlighted pos = Vec.any (\(CursorPosition x y) -> x == pos && y == rowIndex) positionsToHighlight

charToImage :: Char -> Bool -> Vty.Image
charToImage c highlight = Vty.char attr c
  where
    attr = if highlight
           then Vty.defAttr `Vty.withForeColor` Vty.red
           else Vty.defAttr `Vty.withForeColor` Vty.blue

findWordPositions :: Vec.Vector Char -> Vec.Vector (Vec.Vector Char) -> Vec.Vector (Vec.Vector CursorPosition)
findWordPositions word = Vec.imap (findWordInRow 0)
  where
    findWordInRow :: Int -> Int -> Vec.Vector Char -> Vec.Vector CursorPosition
    findWordInRow col rowIndex row
      | col >= Vec.length row = Vec.empty  -- End of row, no more words to find
      | otherwise =
          let extractedWord = extractWordAt col row
              wordEndIndex = col + Vec.length extractedWord - 1
          in if extractedWord == word
             then createPositions col wordEndIndex rowIndex Vec.++ findWordInRow (wordEndIndex + 1) rowIndex row
             else findWordInRow (col + 1) rowIndex row
          --   

createPositions :: Int -> Int -> Int -> Vec.Vector CursorPosition
createPositions x1 x2 y = Vec.generate (x2 - x1 + 1) (\i -> CursorPosition (x1 + i) y)

extractWordAt :: Int -> Vec.Vector Char -> Vec.Vector Char
extractWordAt x row = word
  where
    (before, after) = Vec.splitAt x row
    findIndexUntilSpace word = Vec.length (Vec.takeWhile (/= ' ') word)
    startOffset = findIndexUntilSpace $ Vec.reverse before
    endOffset = findIndexUntilSpace after
    startIndex = x - startOffset
    endIndex = x + endOffset
    word = Vec.take endIndex (Vec.drop startIndex row)

createImageText :: Vec.Vector (Vec.Vector Char) -> Vty.Image
createImageText buffer = Vty.vertCat $ Vec.toList $ Vec.imap rowToImage buffer
  where
    rowToImage :: Int -> Vec.Vector Char -> Vty.Image
    -- rowToImage index row = displayText (show index ++ " ") Vty.<|> displayText (Vec.toList row)
    rowToImage index row = displayText (Vec.toList row)

createImageRow :: Vec.Vector (Vec.Vector Char) -> [CursorPosition] -> Vty.Image
createImageRow buffer sortedPositionsToHighlight = Vty.vertCat $ Vec.toList $ Vec.imap rowToImage buffer
  where
    rowToImage :: Int -> Vec.Vector Char -> Vty.Image
    rowToImage index row = displayText (show index ++ " ") Vty.<|> displayText (Vec.toList row)


createImageForReadFileMode :: Vec.Vector (Vec.Vector Char) -> String -> String -> Vty.Image
createImageForReadFileMode buffer title text =
  let titleBarImg = createTitleBar title
      textImg = displayText text
      contentImg = Vty.vertCat $ Vec.toList $ Vec.map rowToImage buffer
   in Vty.vertCat [titleBarImg, textImg, contentImg]
  where
    rowToImage :: Vec.Vector Char -> Vty.Image
    rowToImage row = displayText (Vec.toList row)

createTitleBar :: String -> Vty.Image
createTitleBar = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.green)

displayText :: String -> Vty.Image
displayText = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.yellow)

getWindowConfig :: Vty.Vty -> IO Window
getWindowConfig vty = do
  let output = Vty.outputIface vty
  (width, height) <- Vty.displayBounds output
  return Window {width = width, height = height}

marginConfigEditMode :: Margin
marginConfigEditMode = Margin {left = 2, right = 0, top = 1, bottom = 0}

marginConfigReadFileMode :: Margin
marginConfigReadFileMode = Margin {left = 0, right = 0, top = 2, bottom = 0}

editFileModeImageTitle :: String -> String
editFileModeImageTitle filePath = "[Edit Mode] " ++ fileName filePath ++ "[Load File (Ctrl-o)] [Save File (Ctrl-s)] [Exit Editor (Ctrl-c)]"
  where
    fileName :: String -> String
    fileName [] = ""
    fileName x = "[File " ++ x ++ "] "

writeFileModeImageTitle :: String
writeFileModeImageTitle = "[Read File Mode] [Load File (Enter)] [Switch to Edit Mode (Esc)]"

writeFileModeImageText :: String
writeFileModeImageText = "Save at: "

readFileModeImageTitle :: String
readFileModeImageTitle = "[Read File Mode] [Load File (Enter)] [Switch to Edit Mode (Esc)]"

readFileModeImageText :: String
readFileModeImageText = "Enter Path: "
