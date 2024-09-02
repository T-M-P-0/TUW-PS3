module AppState (AppState (..), CursorPosition (..), Window (..), Mode (..)) where

import Data.Vector

data CursorPosition = CursorPosition {x :: Int, y :: Int} deriving (Show)

data Window = Window {width :: Int, height :: Int} deriving (Show)

data Mode = EditFile | ReadFile | WriteFile deriving (Show, Eq)

data AppState = AppState {buffer :: Vector (Vector Char), cursorPosition :: CursorPosition, window :: Window, mode :: Mode, filePath :: String, previousBuffer:: Vector (Vector Char), errors:: [(Int, String)], exit :: Bool} deriving (Show)