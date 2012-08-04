"set compatible
let mapleader = ","
let maplocalleader = "\\"

if !isdirectory(expand("~/.vim/bundle/vundle"))
    !git clone git://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endif

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/vimproc'
Bundle 'Shougo/vimshell'
Bundle 'chrisbra/SudoEdit.vim'
Bundle 'godlygeek/tabular'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/FuzzyFinder'
Bundle 'vim-scripts/L9'

Bundle 'dag/vim2hs'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'ujihisa/neco-ghc'

Bundle 'oscarh/vimerl'
Bundle 'tpope/vim-markdown'
Bundle 'vim-scripts/nginx.vim'
Bundle 'vim-scripts/JSON.vim'

let g:erlangCompletionDisplayDoc = 0
let g:erlangFoldSplitFunction = 1
let g:haskell_force_sane_indentation = 1

au BufEnter *.hs compiler ghc
let g:haddock_browser = 'echo'
let g:haskell_conceal = 0

let s:begin_directory_match = '|(^|[/\\])'
let s:end_directory_match   = '($|[/\\])'
let s:begin_extension_match = '|\.('
let s:end_extension_match   = ')$'

let g:ignored_files = '\v\~$'
       \ . s:begin_extension_match
       \ . 'bak|swp|orig|test'
       \ . '|jpg|png|psd|gif|zip'
       \ . '|hi|p_hi|p_o|chi|chs\.h'
       \ . '|annot|cmo|cma|cmi|cmx|cmxa'
       \ . '|o|lo|slo|a|la|sla|lib|so|dylib'
       \ . '|exe|dll|beam'
       \ . s:end_extension_match
       \ . s:begin_directory_match
       \ . '\.(hg|git|bzr|svn)'
       \ . '|dist|cabal-dev|\.virthualenv'
       \ . s:end_directory_match
let g:fuf_file_exclude = g:ignored_files
let g:fuf_coveragefile_exclude = g:ignored_files

let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1

inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<TAB>"

imap <F1> <nop>
map <F1> <nop>
map <F2> :NERDTreeToggle<cr>
map <F3> :FufBuffer<cr>
map <F4> :FufFile<cr>
map <F5> :FufCoverageFile<cr>
map <F6> :FufRenewCache<cr>

set wildmode=longest,list,full
set wildmenu

autocmd BufRead *.as    set filetype=actionscript
autocmd BufRead *.inf   set filetype=dosini
autocmd BufRead *.json  set filetype=json

autocmd BufRead *.scons    set filetype=python
autocmd BufRead SConstruct set filetype=python autocmd BufRead SCsub      set filetype=python

autocmd BufRead *.roy      set filetype=ocaml

filetype plugin on
filetype indent on
syntax on

set hlsearch
set nobackup
set ruler
set showcmd

set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

set colorcolumn=81
set cursorline
set modeline

set encoding=utf-8
set fileformat=unix

set backspace=indent,eol,start
set directory=~/.vim/tmp/swap//
set mousemodel=popup
set shortmess+=I
set undodir=~/.vim/tmp/undo//

function! StripTrailingWhite()
    let l:winview = winsaveview()
    silent! %s/\s\+$//
    call winrestview(l:winview)
endfunction

function! SourceFile()
    if &l:filetype !=# 'markdown'
        call StripTrailingWhite()
        autocmd BufWritePre <buffer> :call StripTrailingWhite()
    endif
    setlocal undofile
endfunction

command! Strip :call StripTrailingWhite()

autocmd FileType ocaml,haskell,c,cpp,vim,python,php,markdown :call SourceFile()

if has('python')
    Bundle 'SirVer/ultisnips'
    Bundle 'sjl/gundo.vim'
    map <F1> :GundoToggle<cr>
endif

let g:solarized_bold = 0
let g:solarized_italic = 0
let g:solarized_underline = 0
let g:solarized_termcolors = 256

if has('gui_running')
    if has('gui_win32')
        set gfn=ProfontWindows
    elseif has('gui_macvim')
        set gfn=ProFontX:h9
        set noantialias
    else
        set gfn=ProfontWindows\ 9
    endif
    set guioptions-=m
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
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

if filereadable(expand("~/.vim/local.vim"))
    source ~/.vim/local.vim
endif
