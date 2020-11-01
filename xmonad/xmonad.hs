{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}

import Control.Monad (unless)
import Data.Default (def)
import Data.Int (Int32)
import Data.Monoid (All (All))
import Data.Word (Word32)
import XMonad
import XMonad.Hooks.SetWMName (setWMName)

import qualified Data.Map as Map
import qualified XMonad as Xm
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Layout.NoFrillsDecoration as Nf
import qualified XMonad.Layout.SimpleDecoration as Sd
import qualified XMonad.Util.Themes as Th
import qualified XMonad.Layout.DecorationMadness as Dm

main = xmonad $ def
    { terminal = "xterm"
    , modMask  = mod4Mask
    , borderWidth = 1
    , keys = keymap
    , mouseBindings = mousemap
    , handleEventHook = handle_event
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , focusFollowsMouse  = False
    , clickJustFocuses = False
    , startupHook = do
        setWMName "LG3D"
    -- , layoutHook = myL
    }

dmenu = "exe=`dmenu_path | dmenu -fn 'Tahoma-8' " ++
                                "-nb '#201f1f' " ++
                                "-nf '#d4d2cf' " ++
                                "-sb '#184880' " ++
                                "-sf '#ffffff'`" ++
        "&& eval \"exec $exe\""

{-
myL = deco Sd.shrinkText theme -- (layoutHook def)
  where
    -- deco = Nf.noFrillsDeco
    -- deco = Sd.simpleDeco
    deco = Dm.floatSimple
    theme = Th.theme Th.adwaitaDarkTheme
-}

noop = pure ()

keymap :: Xm.XConfig Xm.Layout -> Map.Map (Xm.KeyMask, Xm.KeySym) (Xm.X ())
keymap cfg = Map.union customKeys (keys def cfg)
  where
    mmask = modMask cfg
    customKeys :: Map.Map (Xm.KeyMask, Xm.KeySym) (Xm.X ())
    customKeys = Map.fromList
        [ ((mmask, xK_p), noop {- spawn dmenu -})
        ]

mousemap :: Xm.XConfig Xm.Layout -> Map.Map (Xm.KeyMask, Xm.Button) (Xm.Window -> Xm.X ())
mousemap cfg = Map.union bindings (mouseBindings def cfg)
  where
    mmask = modMask cfg
    bindings = Map.fromList
        [ ((mmask, button1), mouse_move_window)
        , ((mmask, button3), mouse_resize_window)
        ]

handle_event :: Xm.Event -> Xm.X All
handle_event evt = case evt of
    ButtonEvent { ev_window = w, ev_event_type = t, ev_button = b } | t == buttonPress -> do
        dpy <- asks display
        isr <- isRoot w
        m <- cleanMask $ ev_state evt
        mact <- asks (Map.lookup (m, b) . buttonActions)
        case mact of
            Just act | isr -> do
                act $ ev_subwindow evt
            _ -> do
                focus w
                windows StackSet.shiftMaster
                ctf <- asks (clickJustFocuses . config)
                unless ctf $ io (allowEvents dpy replayPointer currentTime)
        broadcastMessage evt -- Always send button events.
        pure (All False)
    _ -> do
        pure (All True)


mouse_move_window :: Window -> X ()
mouse_move_window wnd = whenX (isClient wnd) $ withDisplay $ \display -> do
    -- io $ raiseWindow display wnd
    attributes <- io $ getWindowAttributes display wnd
    (_, _, _, ox', oy', _, _, _) <- io $ queryPointer display wnd
    let ox = fromIntegral ox'
    let oy = fromIntegral oy'
    let drag_action ex ey = do
            io $ moveWindow display wnd
                (fromIntegral (fromIntegral (wa_x attributes) + (ex - ox)))
                (fromIntegral (fromIntegral (wa_y attributes) + (ey - oy)))
            float wnd
    mouseDrag drag_action (float wnd)

data Third = FirstThird | MiddleThird | LastThird

to_thirds val third_size
    | val < third_size = FirstThird
    | val > third_size * 2 = LastThird
    | otherwise = MiddleThird

adjust_dimension start length delta third = case third of
    FirstThird -> (start + delta, length - delta)
    MiddleThird -> (start, length)
    LastThird -> (start, length + delta)

-- | resize the window under the cursor with the mouse while it is dragged
mouse_resize_window :: Window -> X ()
mouse_resize_window wnd = whenX (isClient wnd) $ withDisplay $ \display -> do
    -- io $ raiseWindow display wnd
    attributes <- io $ getWindowAttributes display wnd
    size_hints <- io $ getWMNormalHints display wnd
    (ox, oy) <- do
        (_, _, _, ox', oy', _, _, _) <- io $ queryPointer display wnd
        pure (fromIntegral ox', fromIntegral oy')
    let left :: Int32 = fromIntegral $ wa_x attributes
    let top :: Int32 = fromIntegral $ wa_y attributes
    let width :: Int32 = fromIntegral $ wa_width attributes
    let height :: Int32 = fromIntegral $ wa_height attributes
    let width_third = width `div` 3
    let height_third = width `div` 3
    let (relx, rely) = (ox - left, oy - top)
    let wthird = to_thirds relx width_third
    let hthird = to_thirds rely height_third
    io $ warpPointer display none wnd 0 0 0 0 width height
    let drag_action ex ey = do
            let (dx, dy) = (ex - ox, ey - oy)
            let (proposed_left, proposed_width) = adjust_dimension left width dx wthird
            let (proposed_top, proposed_height) = adjust_dimension top height dy hthird
            let (w, h) = applySizeHintsContents size_hints (proposed_width, proposed_height)
            io $ moveResizeWindow display wnd proposed_left proposed_top w h
            float wnd
    mouseDrag drag_action (float wnd)
