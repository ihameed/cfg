#!/bin/zsh
_gvim="$HOME/.local/programs/vim/gvim.exe"
if [ $# -eq 0 ]; then
    cygstart "$_gvim"
elif [ "$1" = "-" ]; then
    "$_gvim" - &
    disown
else
    "$_gvim" "$(cygpath -w "$1")" &
    disown
fi
