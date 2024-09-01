module CommandHandler (commandHandler) where

import AppState (AppState (..), CursorPosition (..), Window (..))
import Command (Command (..))
import qualified Data.Vector as Vec
import VectorUtils (deleteChar, insertChar)
import Graphics.Vty (text, Vty (update))

type CommandHandler = AppState -> IO AppState

commandHandler :: Maybe Command -> CommandHandler
commandHandler (Just (InsertChar c)) =
  \(AppState buffer cursor@(CursorPosition cursorPosX cursorPosY) window@(Window width _) exit) -> do
    let updatedBuffer = insertChar c cursorPosY cursorPosX buffer
    return
      AppState
        { buffer = updatedBuffer,
          cursorPosition = incrementCursorX cursor width updatedBuffer,
          window = window,
          exit = False
        }
commandHandler (Just DeleteChar) =
  \(AppState buffer cursorPos@(CursorPosition x y) window exit) -> do
    let updatedBuffer = deleteChar x y buffer
    let updatedCursorPos = decrementCursorX cursorPos updatedBuffer
    let filteredBuffer = removeRowIfEmpty y updatedBuffer
    -- If the row where the element was removed does not contain any elements anymore we just remove the row
    let
    return
      AppState
        { buffer = filteredBuffer,
          cursorPosition = updatedCursorPos,
          window = window,
          exit = False
        }

commandHandler (Just NewLine) =
  \(AppState buffer cursorPos window@(Window width height) exit) ->
    do
      let updatedBuffer = Vec.snoc buffer (Vec.fromList [])
      return
        AppState
          { buffer = updatedBuffer,
            cursorPosition = incrementCursorY cursorPos height updatedBuffer,
            window = window,
            exit = False
          }
commandHandler (Just MoveCursorLeft) =
  \(AppState buffer cursorPos@(CursorPosition cursorPosX cursorPosY) window@(Window width height) exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = decrementCursorX cursorPos buffer,
          window = window,
          exit = False
        }
commandHandler (Just MoveCursorRight) =
  \(AppState buffer cursorPos@(CursorPosition cursorPosX cursorPosY) window@(Window width height) exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = incrementCursorX cursorPos width buffer,
          window = window,
          exit = False
        }
commandHandler (Just MoveCursorUp) =
  \(AppState buffer cursorPos window exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = decrementCursorY cursorPos buffer,
          window = window,
          exit = False
        }
commandHandler (Just MoveCursorDown) =
  \(AppState buffer cursorPos@(CursorPosition cursorPosX cursorPosY) window@(Window _ height) exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = incrementCursorY cursorPos height buffer,
          window = window,
          exit = False
        }
commandHandler (Just SaveFile) = \s -> do
  -- Implement saving logic here
  putStrLn "File saved!"
  return s
commandHandler (Just Exit) = \(AppState buffer cursorPosition window exit) ->
  return
    AppState
      { buffer = buffer,
        cursorPosition = cursorPosition,
        window = window,
        exit = True
      }

incrementCursorX :: CursorPosition -> Int -> Vec.Vector (Vec.Vector Char) -> CursorPosition
incrementCursorX cursorPos@(CursorPosition x y) boundary textBuffer = if x + 1 > boundary || (x + 1 > Vec.length (textBuffer Vec.! y)) then adjustedOutOfBoundsCursorPos else CursorPosition (x + 1) y
  where
    adjustedOutOfBoundsCursorPos = if y + 1 >= Vec.length textBuffer then CursorPosition x y else  CursorPosition 0 (y +1)

incrementCursorY :: CursorPosition -> Int -> Vec.Vector (Vec.Vector Char) -> CursorPosition
incrementCursorY cursorPos@(CursorPosition x y) boundary textBuffer = if y + 1 >= boundary || (y + 1 >= (Vec.length textBuffer)) then cursorPos else CursorPosition adjustedX (y + 1)
  where
    rowLength = Vec.length (textBuffer Vec.! (y + 1))
    adjustedX = min x rowLength

decrementCursorX :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> CursorPosition
decrementCursorX cursorPos@(CursorPosition x y) textBuffer = if x - 1 < 0 then adjustedOutOfBoundsCursorPos else CursorPosition {x = x - 1, y = y}
  where
    adjustedOutOfBoundsCursorPos = if y - 1 >= 0 then CursorPosition (length (textBuffer Vec.! (y - 1))) (y -1) else cursorPos

decrementCursorY :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> CursorPosition
decrementCursorY cursorPos@(CursorPosition x y) textBuffer = if y - 1 < 0 then cursorPos else CursorPosition {x = adjustedX, y = y - 1}
  where
    rowLength = Vec.length (textBuffer Vec.! (y - 1))
    adjustedX = min x rowLength

removeRowIfEmpty :: Int -> Vec.Vector (Vec.Vector Char) -> Vec.Vector (Vec.Vector Char)
removeRowIfEmpty currentRow = Vec.ifilter (\index element -> not (Vec.null element) || index == 0 || index /= currentRow)