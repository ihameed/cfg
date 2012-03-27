"set compatible
set vb t_vb=
set t_Co=256
set cpo-=C

colorscheme wombat256

set wildmode=longest,list,full
set wildmenu

autocmd BufRead *.as    set filetype=actionscript
autocmd BufRead *.json  set filetype=json
autocmd BufRead *.inf   set filetype=dosini

autocmd BufRead SConstruct set filetype=python
autocmd BufRead SCsub      set filetype=python
autocmd BufRead *.scons    set filetype=python

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

function! OCamlType()
    let col  = col('.')
    let line = line('.')
    let file = expand("%:p:r")
    echo system("annot -n -type ".line." ".col." ".file.".annot")
endfunction
map <Esc>t :call OCamlType()<cr>

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'oscarh/vimerl'
Bundle 'scrooloose/nerdtree'
Bundle 'vim-scripts/L9'
Bundle 'vim-scripts/FuzzyFinder'

" :-(
Bundle 'paulyg/Vim-PHP-Stuff'

let g:erlangCompletionGrep='zgrep'
let g:erlangManSuffix='erl\.gz'

map <F2> :NERDTreeToggle<cr>
map <F3> :FufBuffer<cr>
map <F4> :FufFile<cr>
map <F5> :FufCoverageFile<cr>
