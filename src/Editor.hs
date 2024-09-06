module Editor (start) where

import qualified Data.Vector as Vec
import Data.List ( intercalate )
import qualified Graphics.Vty as Vty
import ExtensionHelper
    ( findMatchingBrace, isBrace, findWordPositions, extractWordAt )
import SyntaxAnalyzer as Analyzer (execute, Result(..),Error(..))
import Graphics.Vty (Cursor (Cursor))
import AppState (AppState (..), CursorPosition (..), Mode (..), Window (..))
import Command (Command (..))
import CommandHandler (commandHandler)
import CommandParser (parse)
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
      result <- Analyzer.execute $ vector2DToString buffer
      case result of
        Success -> do
           let (Margin left right top bottom) = marginConfigEditMode 
           Vty.update vty (Vty.picForImage $ Vty.horizCat [img]) {Vty.picCursor = Vty.Cursor (x) y}
        Failure err -> do
           let imageError = createErrorImage (Vec.length buffer) err 
           Vty.update vty (Vty.picForImage $ Vty.vertCat [img, imageError]) {Vty.picCursor = Vty.Cursor (x) y}


createErrorImage :: Int ->  Error  -> Vty.Image
createErrorImage textLength (LineError num errorText) = errorRow
  where
    errorRow = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.red) (errorText ++ "[ LineNr. " ++ show num ++ "]")
createErrorImage textLength (GeneralError errorText) = errorRow
  where
    errorRow = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.red) (errorText ++ "[ LineNr. " ++ show textLength ++ "]")

highlight :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> Vty.Image
highlight cursorPos@(CursorPosition x y) buffer
  | x >= Vec.length currRow = createImageText buffer
  | isBrace currChar = highlightBracePositions cursorPos buffer
  | currChar /= ' ' = highlightWordPositions cursorPos buffer
  | otherwise = createImageText buffer
  where
    currRow = buffer Vec.! y
    currChar = currRow Vec.! x

highlightBracePositions :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> Vty.Image
highlightBracePositions cursorPos buffer =
  case matchingBracePos of
    (Just matchingBrace) -> bufferToImage (highlightedCursorPos matchingBrace) buffer
    _ -> createImageText buffer
  where
    matchingBracePos = findMatchingBrace buffer cursorPos
    highlightedCursorPos brace = Vec.fromList [ Vec.fromList [cursorPos, brace]]

highlightWordPositions :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> Vty.Image
highlightWordPositions (CursorPosition cx cy) buffer = highlightedImage
  where
    currentRow = buffer Vec.! cy
    currentWord = extractWordAt cx currentRow
    positionsToHighlight = findWordPositions currentWord buffer
    highlightedImage = bufferToImage positionsToHighlight buffer

bufferToImage :: Vec.Vector (Vec.Vector CursorPosition) -> Vec.Vector (Vec.Vector Char) -> Vty.Image
bufferToImage positionsToHighlight buffer = Vty.vertCat $ Vec.toList $ Vec.imap (\index row -> if Vec.null row then emptyRowToImage else rowToImage index row flattenPositions) buffer
  where flattenPositions= Vec.concat $ Vec.toList positionsToHighlight
        emptyRowToImage = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.white) ""

rowToImage :: Int -> Vec.Vector Char -> Vec.Vector CursorPosition -> Vty.Image
rowToImage rowIndex row positionsToHighlight = Vty.horizCat $ Vec.toList $ Vec.imap charToImageRow row
  where
    charToImageRow :: Int -> Char -> Vty.Image
    charToImageRow colIndex char = charToImage char (isHighlighted colIndex)
    isHighlighted :: Int -> Bool
    isHighlighted pos = Vec.any (\(CursorPosition x y) -> x == pos && y == rowIndex) positionsToHighlight

charToImage :: Char -> Bool -> Vty.Image
charToImage c highlight = Vty.char attr c
  where
    attr =
      if highlight
        then Vty.defAttr `Vty.withForeColor` Vty.red
        else Vty.defAttr `Vty.withForeColor` Vty.white

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

vector2DToString :: Vec.Vector (Vec.Vector Char) -> String
vector2DToString matrix =
    let rows = Vec.toList matrix
        lines = map (concatMap (:[])) rows
    in intercalate "\n" lines

createTitleBar :: String -> Vty.Image
createTitleBar = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.green)

displayText :: String -> Vty.Image
displayText = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.white)

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
