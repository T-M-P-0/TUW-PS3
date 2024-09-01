module CommandParser (parse) where

import Command (Command (..))
import KeyInfo (Key (..), KeyInfo (..), Modifier (..))

parse :: Maybe KeyInfo -> Maybe Command
parse (Just (KeyInfo Enter None)) = Just NewLine
parse (Just (KeyInfo LeftArrow None)) = Just MoveCursorLeft
parse (Just (KeyInfo RightArrow None)) = Just MoveCursorRight
parse (Just (KeyInfo UpArrow None)) = Just MoveCursorUp
parse (Just (KeyInfo DownArrow None)) = Just MoveCursorDown
parse (Just (KeyInfo BackSpace None)) = Just DeleteChar
parse (Just (KeyInfo (Char c) None)) = Just (InsertChar c)

parse (Just (KeyInfo (Char 'c') Ctrl)) = Just Exit
parse (Just (KeyInfo _ _)) = Nothing
parse Nothing = Nothing