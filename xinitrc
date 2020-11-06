export QT_QPA_PLATFORMTHEME=qt5ct
export QT_AUTO_SCREEN_SCALE_FACTOR=0
setxkbmap -option "terminate:ctrl_alt_bksp"
xrdb "$HOME"/.Xresources
xset m 1 1
#(xscreensaver 2>&1) &
xsetroot -cursor_name left_ptr
xsetroot -solid black
xbindkeys -p
xset r rate 300 30 # 300ms delay, 30 repeat events per second
"$HOME"/.local/bin/set-wallpaper.sh &
vmware-user
#exec xmonad
#startplasma-x11
exec xfwm4
