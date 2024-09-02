module InputReader (readInput) where

import qualified Graphics.Vty as V
import KeyInfo (KeyInfo(..), Key(..), Modifier(..))
import Graphics.Vty (Vty(shutdown))

-- Function to handle input events
handleInput :: V.Event -> Maybe KeyInfo
handleInput (V.EvKey V.KEnter []) =  Just (KeyInfo { key = Enter, modifier = None})
handleInput (V.EvKey V.KBS []) = Just (KeyInfo { key = BackSpace, modifier = None })
handleInput (V.EvKey V.KLeft []) = Just (KeyInfo { key = LeftArrow, modifier = None })
handleInput (V.EvKey V.KRight []) = Just (KeyInfo { key = RightArrow, modifier = None })
handleInput (V.EvKey V.KUp []) = Just (KeyInfo { key = UpArrow, modifier = None })
handleInput (V.EvKey V.KDown []) = Just (KeyInfo { key = DownArrow, modifier = None })
handleInput (V.EvKey (V.KChar '\t') []) = Just (KeyInfo { key = Char '\t', modifier = None})
handleInput (V.EvKey (V.KChar c) []) = Just (KeyInfo { key = Char c, modifier = None})
handleInput (V.EvKey (V.KChar 'c') [V.MCtrl]) = Just (KeyInfo { key = Char 'c', modifier = Ctrl })
handleInput (V.EvKey (V.KChar 'o') [V.MCtrl]) = Just (KeyInfo { key = Char 'o', modifier = Ctrl })
handleInput (V.EvKey (V.KChar 's') [V.MCtrl]) = Just (KeyInfo { key = Char 's', modifier = Ctrl })
handleInput (V.EvKey V.KEsc []) = Just (KeyInfo { key = Esc, modifier = None })
handleInput (V.EvKey _ _) = Nothing

readInput:: V.Vty -> IO (Maybe KeyInfo)
readInput vty = do
  event <- V.nextEvent vty
  return $ handleInput event