module KeyInfo (KeyInfo(..), Key(..), Modifier(..)) where
data Key = Char Char | Enter | BackSpace | LeftArrow | RightArrow | UpArrow | DownArrow deriving(Show, Eq)
data Modifier = None | Ctrl deriving(Show, Eq)

data KeyInfo =  KeyInfo {
  key:: Key, 
  modifier:: Modifier
} deriving(Show, Eq)