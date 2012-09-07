"set compatible

set encoding=utf-8
set fileformat=unix

let mapleader = ","
let maplocalleader = "\\"
let g:guifont = ''

if !isdirectory(expand("~/.vim/bundle/vundle"))
  !git clone git://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
endif

function! InitTmp(dirs)
  for dir in a:dirs
    let absdir = expand('~/.vim/tmp/' . dir)
    if !isdirectory(absdir)
      silent exe '!mkdir ' . absdir
    endif
  endfor
endfunction

call InitTmp(['undo', 'swap', 'ctrlpcache'])

function! VsnCmd(vsn, cmd)
  if version >= a:vsn
    execute a:cmd
  endif
endfunction

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/vimproc'
Bundle 'Shougo/vimshell'
Bundle 'chrisbra/SudoEdit.vim'
Bundle 'godlygeek/tabular'
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'thinca/vim-prettyprint'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'xaimus/vim-togglelist'

Bundle 'dag/vim2hs'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'ujihisa/neco-ghc'

Bundle 'oscarh/vimerl'
Bundle 'tpope/vim-markdown'
Bundle 'vim-scripts/JSON.vim'
Bundle 'vim-scripts/nginx.vim'
Bundle 'proyvind/Cpp11-Syntax-Support'

let g:ignored_dirs = '\v'
                 \ . '\.(hg|git|bzr|svn)'
                 \ . '|dist|cabal-dev|\.virthualenv'

let g:ignored_files = '\v\~|%('
                  \ . 'bak|swp|orig|test'
                  \ . '|jpg|png|psd|gif|zip'
                  \ . '|hi|p_hi|p_o|chi|chs\.h'
                  \ . '|annot|cmo|cma|cmi|cmx|cmxa'
                  \ . '|o|lo|slo|a|la|sla|lib|so|dylib'
                  \ . '|exe|dll|beam'
                  \ . ')$'

let g:erlangCompletionDisplayDoc = 0
let g:erlangFoldSplitFunction = 1
let g:haskell_force_sane_indentation = 1

let g:haddock_browser = 'echo'
let g:haskell_conceal = 0

let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_dictionary_filetype_lists = { 'default': '' }

let g:NERDTreeDirArrows = 1
let g:NERDTreeMinimalUI = 1

let g:ctrlp_cache_dir = expand('~/.vim/tmp/ctrlpcache')
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_map = ''
let g:ctrlp_max_height = 30
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_prompt_mappings = { 'PrtHistory(-1)': [],
                              \ 'PrtHistory(1)':  [],
                              \ 'PrtSelectMove("j")': ['<c-n>'],
                              \ 'PrtSelectMove("k")': ['<c-p>'],
                              \ 'PrtBS()': ['<c-h>', '<bs>', '<c-]>'],
                              \ 'PrtCurLeft()': ['<left>', '<c-^>'],
                              \ }
let g:ctrlp_custom_ignore = { 'dir':  g:ignored_dirs,
                            \ 'file': g:ignored_files,
                            \ 'link': g:ignored_files,
                            \ }


inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<TAB>"

imap <f1> <nop>
map <f1> <nop>
map <f2> <esc>:NERDTreeToggle<cr>
map <f3> <esc>:CtrlPBuffer<cr>
map <f4> <esc>:CtrlP<cr>
map <f5> <esc>:CtrlPClearCache<cr>

autocmd BufRead *.as   set filetype=actionscript
autocmd BufRead *.inf  set filetype=dosini
autocmd BufRead *.json set filetype=json

autocmd BufRead *.scons    set filetype=python
autocmd BufRead SConstruct set filetype=python
autocmd BufRead SCsub      set filetype=python

autocmd BufRead *.roy set filetype=ocaml
autocmd BufRead *.md  set filetype=markdown

autocmd FileType cpp set filetype=cpp11

filetype plugin on
filetype indent on
syntax on

set hlsearch
set ruler
set showcmd
set showmode

set wildmenu
set wildmode=longest,list,full

call VsnCmd(703, 'set colorcolumn=81')
set cursorline
set modeline

set list listchars=tab:»·,trail:·
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=8

set backspace=indent,eol,start
set directory=~/.vim/tmp/swap//
set mousemodel=popup
call VsnCmd(703, 'set undodir=~/.vim/tmp/undo//')

set cedit=
set formatoptions=vt
set nobackup
set notildeop
set shortmess=I
set whichwrap=
set viminfo=

function! FontSet(fontspec)
  execute 'set guifont=' . a:fontspec
  if match(&guifont, 'profont\c') != -1
    set noantialias
  else
    set antialias
  endif
endfunction

function! FontInit(fontspec)
  if has('gui_running') && empty(&guifont)
    call FontSet(a:fontspec)
  endif
endfunction

function! SplitRtp()
  return split(&runtimepath, ',')
endfunction

function! StripTrailingWhite()
  let l:winview = winsaveview()
  silent! %s/\s\+$//
  call winrestview(l:winview)
endfunction

function! ConfigSourceFileBuffer()
  if &l:filetype !=# 'markdown'
    call StripTrailingWhite()
    autocmd BufWritePre <buffer> :call StripTrailingWhite()
  endif
  call VsnCmd(703, 'setlocal undofile')
  NeoComplCacheEnable
endfunction

function! RefreshComplCache()
endfunction

function! CommandCabbr(abbreviation, expansion)
  execute 'cabbr ' . a:abbreviation
      \ . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion
      \ . '" : "' . a:abbreviation . '"<CR>'
endfunction

command! -nargs=0                Strip call StripTrailingWhite()
command! -nargs=0                Make  silent make!
command! -nargs=* -complete=help Help  vert help <args>

call CommandCabbr('help', 'Help')
call CommandCabbr('ty',   'Type')
call CommandCabbr('tyc',  'Ctype')
call CommandCabbr('rc',   'RefreshComplCache')

autocmd FileType ocaml,haskell,cabal,c,cpp,cpp11
               \,vim,python,php,markdown
               \,javascript,json
               \ :call ConfigSourceFileBuffer()


autocmd QuickFixCmdPost [^l]* nested Copen
autocmd QuickFixCmdPost    l* nested Lopen

if has('python')
  Bundle 'SirVer/ultisnips'
  Bundle 'sjl/gundo.vim'
  map <F1> <esc>:GundoToggle<cr>
endif

let g:solarized_bold = 0
let g:solarized_italic = 0
let g:solarized_underline = 0
let g:solarized_termcolors = 256

if has('gui_running')
  if has('gui_win32')
    let g:guifont = 'ProfontWindows'
  elseif has('gui_macvim')
    let g:guifont = 'ProFontX:h9'
  else
    let g:guifont = 'ProfontWindows\ 9'
  endif
  set guioptions-=L
  set guioptions-=T
  set guioptions-=m
  set guioptions-=r
  set guioptions-=e
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

call FontInit(g:guifont)
