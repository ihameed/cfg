#!/usr/bin/env zsh
scriptdir=$(cd -P -- "$(dirname -- "$0")"; pwd -P)
_link() {
    local target="$scriptdir"/"$1"
    local linkpath="$2"
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

_link Xresources ~/.Xresources
_link emacs.d ~/.emacs.d
_link fonts.conf ~/.fonts.conf
_link gvimrc ~/.gvimrc
_link minttyrc ~/.minttyrc
_link screenrc ~/.screenrc
_link startxwinrc ~/.startxwinrc
_link tmux.conf ~/.tmux.conf
_link vim ~/.vim
_link vimrc ~/.vimrc
_link xbindkeysrc.scm ~/.xbindkeysrc.scm
_link xinitrc ~/.xinitrc
_link xmonad ~/.xmonad
_link zshrc ~/.zshrc
