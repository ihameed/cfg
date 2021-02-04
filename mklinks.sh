#!/bin/sh
scriptdir="$(cd -P -- "$(dirname -- "$0")"; pwd -P)"

mkdir -p ~/.local

ln -sn "$scriptdir"/Xresources ~/.local/Xresources
ln -sn "$scriptdir"/bash_profile ~/.bash_profile
ln -sn "$scriptdir"/bashrc ~/.bashrc
ln -sn "$scriptdir"/emacs.d ~/.emacs.d
ln -sn "$scriptdir"/fonts.conf ~/.fonts.conf
ln -sn "$scriptdir"/gvimrc ~/.gvimrc
ln -sn "$scriptdir"/minttyrc ~/.minttyrc
ln -sn "$scriptdir"/screenrc ~/.screenrc
ln -sn "$scriptdir"/startxwinrc ~/.startxwinrc
ln -sn "$scriptdir"/tmux.conf ~/.tmux.conf
ln -sn "$scriptdir"/vim ~/.vim
ln -sn "$scriptdir"/vimrc ~/.vimrc
ln -sn "$scriptdir"/xbindkeysrc.scm ~/.xbindkeysrc.scm
ln -sn "$scriptdir"/xinitrc ~/.xinitrc
ln -sn "$scriptdir"/xmonad ~/.xmonad
ln -sn "$scriptdir"/zshrc ~/.zshrc
