module Margin (Margin(..)) where

data Margin = Margin { left:: Int, right:: Int, top:: Int, bottom:: Int} deriving(Show)