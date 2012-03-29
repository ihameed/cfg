"set compatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'scrooloose/nerdtree'
Bundle 'vim-scripts/L9'
Bundle 'vim-scripts/FuzzyFinder'
Bundle 'tpope/vim-surround'

Bundle 'lukerandall/haskellmode-vim'
Bundle 'wlangstroth/vim-haskell'

Bundle 'oscarh/vimerl'

" :-(
Bundle 'paulyg/Vim-PHP-Stuff'


let g:erlangCompletionGrep='zgrep'
let g:erlangManSuffix='erl\.gz'


au BufEnter *.hs compiler ghc
let g:haddock_browser='echo'


let derf = '\v\~$|\.(hi|o|exe|dll|bak|orig|swp)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])'
let g:fuf_file_exclude = derf
let g:fuf_coveragefile_exclude = derf


map <F2> :NERDTreeToggle<cr>
map <F3> :FufBuffer<cr>
map <F4> :FufFile<cr>
map <F5> :FufCoverageFile<cr>



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

filetype plugin on
filetype indent on
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

function! StripTrailingWhite()
    let l:winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:winview)
endfunction

autocmd FileType ocaml,haskell,c,cpp,vim,python,php :call StripTrailingWhite()
