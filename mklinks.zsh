#!/usr/bin/env zsh
scriptdir=$(cd -P -- "$(dirname -- "$0")"; pwd -P)
_link() {
    local target="$scriptdir"/"$1"
    local linkpath="$2"/"$1"
    printf '%s -> %s\n' "$linkpath" "$target"
    case $OSTYPE in
        cygwin)
            local wintarget=$(cygpath -wa "$target")
            local winlinkpath=$(cygpath -wa "$linkpath")
            printf "    %s -> %s\n" "$winlinkpath" "$wintarget"
            if [[ -d $target ]]; then
                cmd /c mklink /d "$winlinkpath" "$wintarget"
            elif [[ -e $target ]]; then
                cmd /c mklink "$winlinkpath" "$wintarget"
            else
                printf '%s not found!\n' "$target"
                exit 2
            fi
            ;;
        *)
            ln -s "$target" "$linkpath"
            ;;
    esac
}

_link .Xresources ~
_link .emacs.d ~
_link .fonts.conf ~
_link .gvimrc ~
_link .minttyrc ~
_link .misc ~
_link .screenrc ~
_link .startxwinrc ~
_link .tmux.conf ~
_link .vim ~
_link .vimrc ~
_link .xinitrc ~
_link .xmonad ~
_link .zshrc ~
