module CommandHandler (commandHandler) where

import AppState (AppState (..), CursorPosition (..), Mode (..), Window (..))
import Command (Command (..))
import qualified Data.Vector as Vec
import Graphics.Vty (Vty (update), text)
import System.Directory (doesFileExist)
import System.IO (IOMode (WriteMode), hClose, hPutStr, openFile)
import VectorUtils (deleteChar, insertChar)

type CommandHandler = AppState -> IO AppState

commandHandler :: Maybe Command -> CommandHandler
commandHandler (Just (InsertChar c)) =
  \(AppState buffer cursor@(CursorPosition cursorPosX cursorPosY) window@(Window width _) mode filePath previousBuffer errors exit) -> do
    let updatedBuffer = insertChar c cursorPosY cursorPosX buffer
    return
      AppState
        { buffer = updatedBuffer,
          cursorPosition = incrementCursorX cursor width updatedBuffer,
          window = window,
          mode = mode,
          filePath = filePath,
          previousBuffer = previousBuffer,
          errors = errors,
          exit = False
        }
commandHandler (Just DeleteChar) =
  \(AppState buffer cursorPos@(CursorPosition x y) window mode filePath previousBuffer errors exit) -> do
    let (newX, newY, updatedBuffer) = deleteChar x y buffer
    return
      AppState
        { buffer = updatedBuffer,
          cursorPosition = CursorPosition newX newY,
          window = window,
          mode = mode,
          filePath = filePath,
          previousBuffer = previousBuffer,
          errors = errors,
          exit = False
        }
commandHandler (Just NewLine) =
  \(AppState buffer cursorPos@(CursorPosition x y) window@(Window width height) mode filePath previousBuffer errors exit) ->
    do
      let updatedBuffer = addRowAt cursorPos buffer
      let (CursorPosition x y) = incrementCursorY cursorPos height updatedBuffer
      return
        AppState
          { buffer = updatedBuffer,
            cursorPosition = CursorPosition 0 y,
            window = window,
            mode = mode,
            filePath = filePath,
            previousBuffer = previousBuffer,
            errors = errors,
            exit = False
          }
commandHandler (Just MoveCursorLeft) =
  \(AppState buffer cursorPos@(CursorPosition cursorPosX cursorPosY) window@(Window width height) mode filePath previousBuffer errors exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = decrementCursorX cursorPos buffer,
          window = window,
          mode = mode,
          filePath = filePath,
          previousBuffer = previousBuffer,
          errors = errors,
          exit = False
        }
commandHandler (Just MoveCursorRight) =
  \(AppState buffer cursorPos@(CursorPosition cursorPosX cursorPosY) window@(Window width height) mode filePath previousBuffer errors exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = incrementCursorX cursorPos width buffer,
          window = window,
          mode = mode,
          filePath = filePath,
          previousBuffer = previousBuffer,
          errors = errors,
          exit = False
        }
commandHandler (Just MoveCursorUp) =
  \(AppState buffer cursorPos window mode filePath previousBuffer errors exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = decrementCursorY cursorPos buffer,
          window = window,
          mode = mode,
          filePath = filePath,
          previousBuffer = previousBuffer,
          errors = errors,
          exit = False
        }
commandHandler (Just MoveCursorDown) =
  \(AppState buffer cursorPos@(CursorPosition cursorPosX cursorPosY) window@(Window _ height) mode filePath previousBuffer errors exit) ->
    return
      AppState
        { buffer = buffer,
          cursorPosition = incrementCursorY cursorPos height buffer,
          window = window,
          mode = mode,
          filePath = filePath,
          previousBuffer = previousBuffer,
          errors = errors,
          exit = False
        }
commandHandler (Just ActivateReadFileMode) = \(AppState buffer cursorPosition window mode filePath previousBuffer errors exit) -> do
  return
    AppState
      { buffer = Vec.fromList [Vec.fromList ""],
        cursorPosition = CursorPosition 0 0,
        window = window,
        mode = ReadFile,
        filePath = filePath,
        previousBuffer = buffer,
        errors = errors,
        exit = exit
      }
commandHandler (Just ExitReadFileMode) = \state@(AppState buffer cursorPosition window mode filePath previousBuffer errors exit) -> do
  return
    AppState
      { buffer = previousBuffer,
        cursorPosition = CursorPosition 0 0,
        window = window,
        mode = EditFile,
        filePath = filePath,
        previousBuffer = Vec.fromList [Vec.fromList ""],
        errors = errors,
        exit = exit
      }
commandHandler (Just LoadFile) =
  \(AppState buffer cursorPosition window mode filePath previousBuffer errors exit) -> do
    let path = Vec.toList $ buffer Vec.! 0
    fileExists <- doesFileExist path
    if fileExists
      then do
        contents <- readFile path
        let content = Vec.fromList . map Vec.fromList . lines $ contents
        return
          AppState
            { buffer = content,
              cursorPosition = CursorPosition 0 0,
              window = window,
              mode = EditFile,
              filePath = path,
              previousBuffer = previousBuffer,
              errors = errors,
              exit = exit
            }
      else do
        return
          AppState
            { buffer = previousBuffer,
              cursorPosition = CursorPosition 0 0,
              window = window,
              mode = EditFile,
              filePath = filePath,
              previousBuffer = Vec.fromList [Vec.fromList ""],
              errors = errors,
              exit = exit
            }
commandHandler (Just ActivateWriteFileMode) = \state@(AppState buffer cursorPosition window mode filePath previousBuffer errors exit) -> do
  return
    AppState
      { buffer = Vec.fromList [Vec.fromList filePath],
        cursorPosition = CursorPosition 0 0,
        window = window,
        mode = WriteFile,
        filePath = filePath,
        previousBuffer = buffer,
        errors = errors,
        exit = exit
      }
commandHandler (Just ExitWriteFileMode) = \state@(AppState buffer cursorPosition window mode filePath previousBuffer errors exit) -> do
  return
    AppState
      { buffer = previousBuffer,
        cursorPosition = CursorPosition 0 0,
        window = window,
        mode = EditFile,
        filePath = filePath,
        previousBuffer = Vec.fromList [Vec.fromList ""],
        errors = errors,
        exit = exit
      }
commandHandler (Just SaveFile) = \state@(AppState buffer (CursorPosition x y) window mode filePath previousBuffer errors exit) -> do
  let path = Vec.toList $ buffer Vec.! y
  let content = unlines $ map Vec.toList $ Vec.toList previousBuffer
  writeFile path content
  return
    AppState
      { buffer = previousBuffer,
        cursorPosition = CursorPosition 0 0,
        window = window,
        mode = EditFile,
        filePath = filePath,
        previousBuffer = Vec.fromList [Vec.fromList ""],
        errors = errors,
        exit = exit
      }
commandHandler (Just Exit) = \(AppState buffer cursorPosition window mode filePath previousBuffer errors exit) ->
  return
    AppState
      { buffer = buffer,
        cursorPosition = cursorPosition,
        window = window,
        mode = ReadFile,
        filePath = filePath,
        previousBuffer = previousBuffer,
        errors = errors,
        exit = True
      }

incrementCursorX :: CursorPosition -> Int -> Vec.Vector (Vec.Vector Char) -> CursorPosition
incrementCursorX cursorPos@(CursorPosition x y) boundary textBuffer = if x + 1 > boundary || (x + 1 > Vec.length (textBuffer Vec.! y)) then adjustedOutOfBoundsCursorPos else CursorPosition (x + 1) y
  where
    adjustedOutOfBoundsCursorPos = if y + 1 >= Vec.length textBuffer then CursorPosition x y else CursorPosition 0 (y + 1)

incrementCursorY :: CursorPosition -> Int -> Vec.Vector (Vec.Vector Char) -> CursorPosition
incrementCursorY cursorPos@(CursorPosition x y) boundary textBuffer = if y + 1 >= boundary || (y + 1 >= Vec.length textBuffer) then cursorPos else CursorPosition adjustedX (y + 1)
  where
    rowLength = Vec.length (textBuffer Vec.! (y + 1))
    adjustedX = min x rowLength

decrementCursorX :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> CursorPosition
decrementCursorX cursorPos@(CursorPosition x y) textBuffer = if x - 1 < 0 then adjustedOutOfBoundsCursorPos else CursorPosition {x = x - 1, y = y}
  where
    adjustedOutOfBoundsCursorPos = if y - 1 >= 0 then CursorPosition (length (textBuffer Vec.! (y - 1))) (y - 1) else cursorPos

decrementCursorY :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> CursorPosition
decrementCursorY cursorPos@(CursorPosition x y) textBuffer = if y - 1 < 0 then cursorPos else CursorPosition {x = adjustedX, y = y - 1}
  where
    rowLength = Vec.length (textBuffer Vec.! (y - 1))
    adjustedX = min x rowLength

addRowAt :: CursorPosition -> Vec.Vector (Vec.Vector Char) -> Vec.Vector (Vec.Vector Char)
addRowAt (CursorPosition posX 0) buffer = Vec.cons leftSideOfRow (Vec.cons rightSideOfRow tail)
  where
    head = Vec.head buffer
    tail = Vec.tail buffer
    (leftSideOfRow, rightSideOfRow) = Vec.splitAt posX head
addRowAt (CursorPosition 0 y) textBuffer = left Vec.++ Vec.cons (Vec.fromList "") right
  where
    (left, right) = Vec.splitAt y textBuffer
addRowAt (CursorPosition x y) textBuffer = left Vec.++ Vec.cons leftSideOfRow updatedRightSide
  where
    (left, right) = Vec.splitAt y textBuffer
    row = textBuffer Vec.! y
    (leftSideOfRow, rightSideOfRow) = Vec.splitAt x row
    updatedRightSide = right Vec.// [(0, rightSideOfRow)]
