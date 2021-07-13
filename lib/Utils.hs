module Utils
  ( altMask, noMask
  ) where

import XMonad


altMask :: KeyMask
altMask = mod1Mask

noMask :: KeyMask
noMask = 0
