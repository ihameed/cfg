import XMonad
import XMonad.Core
import Data.List
import System.IO

main = xmonad $ defaultConfig {
        borderWidth = 2,
        terminal = "konsole",
        normalBorderColor = "#cccccc",
        focusedBorderColor = "#cd8b00"
    }
