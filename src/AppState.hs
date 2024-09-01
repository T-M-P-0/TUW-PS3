module AppState (AppState(..), CursorPosition(..), Window(..)) where

import Data.Vector

data CursorPosition = CursorPosition { x:: Int, y:: Int } deriving(Show)

data Window = Window { width:: Int, height:: Int } deriving(Show)

data AppState = AppState { buffer:: Vector (Vector Char), cursorPosition:: CursorPosition, window:: Window, exit:: Bool } deriving(Show)