"set compatible
set vb t_vb=
set t_Co=256
set cpo-=C

colorscheme wombat256

autocmd BufRead *.as set filetype=actionscript
autocmd BufRead *.scons set filetype=python
filetype indent plugin on
syntax on
set ruler

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set modeline

set hlsearch

set cursorline
