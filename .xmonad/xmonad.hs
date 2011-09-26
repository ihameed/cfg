import XMonad
import Data.Map
import System.IO
import qualified Data.Map as Map

main = xmonad $ defaultConfig {
        terminal = "konsole",
        modMask  = mod4Mask,
        borderWidth = 1,
        XMonad.keys = keymap,
        normalBorderColor  = "#cccccc",
        focusedBorderColor = "#cd8b00",
        focusFollowsMouse  = True
    }

dmenu_cmd = "exe=`dmenu_path | dmenu -fn 'profontwindows-7' " ++
                                    "-nb '#201f1f' " ++
                                    "-nf '#d4d2cf' " ++
                                    "-sb '#184880' " ++
                                    "-sf '#ffffff'`" ++
            "&& eval \"exec $exe\""

unbound_keys x = 
    [
    (modMask x, xK_p)
    ]

custom_keys x =
    [
    ( (modMask x, xK_p), spawn dmenu_cmd )
    ]

foldr_apply f acc l x = foldr f (acc x) (l x)
default_keymap = XMonad.keys defaultConfig
clean_keymap   = foldr_apply Map.delete default_keymap unbound_keys
keymap         = foldr_apply (uncurry Map.insert) clean_keymap custom_keys
