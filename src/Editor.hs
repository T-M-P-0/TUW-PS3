module Editor (start) where

import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import AppState (AppState (..), CursorPosition (..), Window (..))
import KeyInfo (KeyInfo(..))
import InputReader (readInput)
import CommandParser (parse)
import CommandHandler (commandHandler)
import Graphics.Vty (Cursor(Cursor))

start :: IO ()
start = do
   vty <- V.mkVty V.defaultConfig
   window <- getWindowSize vty
   mainLoop (AppState { buffer = Vec.fromList [ Vec.fromList ""] , cursorPosition = CursorPosition {x = 0, y = 0}, window = window, exit = False }) vty

mainLoop :: AppState -> V.Vty -> IO ()
mainLoop state@(AppState _ (CursorPosition x y) _ True) vty = do
  V.shutdown vty
  putStrLn $ "Exit editor" ++ show state
mainLoop state vty = do
  newState <- handleUpdate state vty
  mainLoop newState vty

handleUpdate :: AppState -> V.Vty -> IO AppState
handleUpdate state@(AppState buffer cursorPosition window False) vty = do 
  updateDisplay vty state
  maybeKeyInfo <- readInput vty
  let command =  parse maybeKeyInfo
  case command of
    Just _ -> do
      currWindowSize <- getWindowSize vty
      commandHandler command (AppState buffer cursorPosition currWindowSize False)
    Nothing ->
      return state

vectorToImage :: Vec.Vector (Vec.Vector Char) -> V.Image
vectorToImage grid =
  V.vertCat $ Vec.toList $ Vec.map rowToImage grid
  where
    rowToImage :: Vec.Vector Char -> V.Image
    rowToImage row = displayText $ Vec.toList row

getWindowSize :: V.Vty -> IO Window
getWindowSize vty = do
    let output = V.outputIface vty
    (width, height) <- V.displayBounds output
    return Window { width = width, height = height }

loadText :: FilePath -> IO String
loadText filePath = readFile filePath

saveText :: FilePath -> String -> IO ()
saveText filePath textBuffer = writeFile filePath textBuffer

updateDisplay :: V.Vty -> AppState -> IO ()
updateDisplay vty (AppState buffer (CursorPosition x y) _ _) = do
  let img = vectorToImage buffer

  V.update vty (V.picForImage img) { V.picCursor = V.Cursor x y }

displayText :: String -> V.Image
displayText txt = V.string (V.defAttr `V.withForeColor` V.red) txt