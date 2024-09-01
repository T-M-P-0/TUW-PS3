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
  | SaveFile
  | Exit
  deriving (Show, Eq)