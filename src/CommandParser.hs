module CommandParser (parse) where

import AppState (Mode (..))
import Command (Command (..))
import KeyInfo (Key (..), KeyInfo (..), Modifier (..))

parse :: Maybe KeyInfo -> Mode -> Maybe Command
parse (Just (KeyInfo Enter None)) EditFile = Just NewLine
parse (Just (KeyInfo Enter None)) ReadFile = Just LoadFile 
parse (Just (KeyInfo Enter None)) WriteFile = Just SaveFile 
parse (Just (KeyInfo LeftArrow None)) _ = Just MoveCursorLeft
parse (Just (KeyInfo RightArrow None)) _ = Just MoveCursorRight
parse (Just (KeyInfo UpArrow None)) _ = Just MoveCursorUp
parse (Just (KeyInfo DownArrow None)) _ = Just MoveCursorDown
parse (Just (KeyInfo BackSpace None)) _ = Just DeleteChar
parse (Just (KeyInfo (Char c) None)) _ = Just (InsertChar c)
parse (Just (KeyInfo (Char 'o') Ctrl)) EditFile = Just ActivateReadFileMode
parse (Just (KeyInfo (Char 's') Ctrl)) EditFile = Just ActivateWriteFileMode
parse (Just (KeyInfo Esc _)) ReadFile = Just ExitReadFileMode
parse (Just (KeyInfo Esc _)) WriteFile = Just ExitWriteFileMode
parse (Just (KeyInfo (Char 'c') Ctrl)) _ = Just Exit
parse (Just (KeyInfo _ _)) _= Nothing
parse Nothing _ = Nothing