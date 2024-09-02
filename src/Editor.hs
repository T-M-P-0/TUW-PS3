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
mainLoop state@(AppState _ (CursorPosition x y) _  _ _ _ _ True) vty = do
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
updateDisplay vty (AppState buffer (CursorPosition x y) (Window width height) mode filePath previousBuffer errors exit) = do
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
      let img = createImageWithTitle buffer (editFileModeImageTitle filePath)
      let (Margin left right top bottom) = marginConfigEditMode
      Vty.update vty (Vty.picForImage img) {Vty.picCursor = Vty.Cursor (x + left) (y + top)}

    
createImageWithTitle :: Vec.Vector (Vec.Vector Char) ->  String -> Vty.Image
createImageWithTitle buffer title  =
  let titleBar = createTitleBar title
      content = Vty.vertCat $ Vec.toList $ Vec.imap rowToImage buffer
   in Vty.vertCat [titleBar, content]
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
displayText = Vty.string (Vty.defAttr `Vty.withForeColor` Vty.red)

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
