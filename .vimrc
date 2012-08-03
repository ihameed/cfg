"set compatible
let mapleader = ","
let maplocalleader = "\\"

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'scrooloose/nerdtree'
Bundle 'vim-scripts/L9'
Bundle 'vim-scripts/FuzzyFinder'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'

Bundle 'Shougo/vimproc'
Bundle 'Shougo/neocomplcache'

Bundle 'eagletmt/ghcmod-vim'
Bundle 'eagletmt/ghci-vim'
Bundle 'ujihisa/neco-ghc'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'wlangstroth/vim-haskell'
Bundle 'Twinside/vim-syntax-haskell-cabal'

Bundle 'oscarh/vimerl'

Bundle 'mileszs/ack.vim'

" :-(
Bundle 'paulyg/Vim-PHP-Stuff'


let g:erlangCompletionDisplayDoc=0
let g:erlangFoldSplitFunction=1

au BufEnter *.hs compiler ghc
let g:haddock_browser='echo'


let derf = '\v\~$'
       \ . '|\.(hi|o|p_hi|p_o|exe|dll|bak|beam|orig|swp|test|jpg|png|svn-base|psd|gif|zip)$'
       \ . '|(^|[/\\])\.(hg|git|bzr|.svn)($|[/\\])'
let g:fuf_file_exclude = derf
let g:fuf_coveragefile_exclude = derf

let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1

map <F2> :NERDTreeToggle<cr>
map <F3> :FufBuffer<cr>
map <F4> :FufFile<cr>
map <F5> :FufCoverageFile<cr>
map <F6> :FufRenewCache<cr>



set vb t_vb=
set t_Co=256
set cpo-=C

colorscheme wombat256

set wildmode=longest,list,full
set wildmenu

autocmd BufRead *.as    set filetype=actionscript
autocmd BufRead *.json  set filetype=json
autocmd BufRead *.inf   set filetype=dosini

autocmd BufRead SConstruct set filetype=python autocmd BufRead SCsub      set filetype=python
autocmd BufRead *.scons    set filetype=python

autocmd BufRead *.roy      set filetype=ocaml
autocmd BufRead *.cabal    set filetype=cabal

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
set showcmd
set colorcolumn=80

set mousemodel=popup

function! StripTrailingWhite()
    let l:winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:winview)
endfunction

autocmd FileType ocaml,haskell,c,cpp,vim,python,php :call StripTrailingWhite()
