import XMonad
import qualified Data.Map as Map

main = xmonad $ defaultConfig
    { terminal = "konsole"
    , modMask  = mod4Mask
    , borderWidth = 1
    , keys = keymap
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , focusFollowsMouse  = True
    }

dmenu = "exe=`dmenu_path | dmenu -fn 'profontwindows-7' " ++
                                "-nb '#201f1f' " ++
                                "-nf '#d4d2cf' " ++
                                "-sb '#184880' " ++
                                "-sf '#ffffff'`" ++
        "&& eval \"exec $exe\""

keymap cfg = Map.union customKeys defaultKeymap
  where
    defaultKeymap = keys defaultConfig cfg
    customKeys = Map.fromList
        [ ( (modMask cfg, xK_p), spawn dmenu )
        ]
