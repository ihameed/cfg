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
set enc=utf-8

set mousemodel=popup

function! StripTrailingWhite()
    let l:winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:winview)
endfunction

autocmd FileType ocaml,haskell,c,cpp,vim,python,php :call StripTrailingWhite()

let g:solarized_bold = 0
let g:solarized_italic = 0
let g:solarized_underline = 0

if has('gui_running')
    if has('gui_win32')
        set gfn=ProfontWindows
    elseif has('gui_macvim')
        set gfn=ProFontX:h9
        set noantialias
    else
        set gfn=ProfontWindows\ 9
    endif
    set guioptions-=m  "remove menu bar
    set guioptions-=T  "remove toolbar
    set guioptions-=r  "remove right-hand scroll bar
    set guioptions-=L
    set lines=75 columns=150
    set number
    set linespace=1

    set background=light
    colorscheme solarized
else
    set vb t_vb=
    set t_Co=256
    set cpo-=C

    colorscheme wombat256
endif
