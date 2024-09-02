module Command (Command(..)) where

data Command
  = InsertChar Char
  | DeleteChar
  | NewLine
  | Tab
  | MoveCursorLeft
  | MoveCursorRight
  | MoveCursorUp
  | MoveCursorDown
  | ActivateReadFileMode
  | ActivateWriteFileMode
  | ExitReadFileMode
  | ExitWriteFileMode
  | LoadFile
  | SaveFile
  | Exit
  deriving (Show, Eq)