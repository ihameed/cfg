#!/bin/sh
scriptdir="$(cd -P -- "$(dirname -- "$0")"; pwd -P)"

ln -s "$scriptdir"/Xresources ~/.Xresources
ln -s "$scriptdir"/bash_profile ~/.bash_profile
ln -s "$scriptdir"/bashrc ~/.bashrc
ln -s "$scriptdir"/emacs.d ~/.emacs.d
ln -s "$scriptdir"/fonts.conf ~/.fonts.conf
ln -s "$scriptdir"/gvimrc ~/.gvimrc
ln -s "$scriptdir"/minttyrc ~/.minttyrc
ln -s "$scriptdir"/screenrc ~/.screenrc
ln -s "$scriptdir"/startxwinrc ~/.startxwinrc
ln -s "$scriptdir"/tmux.conf ~/.tmux.conf
ln -s "$scriptdir"/vim ~/.vim
ln -s "$scriptdir"/vimrc ~/.vimrc
ln -s "$scriptdir"/xbindkeysrc.scm ~/.xbindkeysrc.scm
ln -s "$scriptdir"/xinitrc ~/.xinitrc
ln -s "$scriptdir"/xmonad ~/.xmonad
ln -s "$scriptdir"/zshrc ~/.zshrc
