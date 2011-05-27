import XMonad

import XMonad.Core

import Data.List

import System.IO

main = xmonad $ defaultConfig {
        borderWidth = 2,
        terminal = "urxvt",
        normalBorderColor = "#cccccc",
        focusedBorderColor = "#cd8b00"
    }

