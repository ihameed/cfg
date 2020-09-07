"set compatible

set encoding=utf-8
set fileformat=unix

runtime macros/matchit.vim

let mapleader = ","
let maplocalleader = "\\"
let g:guifont = ''

function! InitTmp(dirs)
  for dir in a:dirs
    let absdir = expand('~/.local/vim/tmp/' . dir)
    if !isdirectory(absdir)
      silent exe '!mkdir -p ' . absdir
    endif
  endfor
endfunction

call InitTmp(['undo', 'swap', 'ctrlpcache'])

function! VsnCmd(vsn, cmd)
  if version >= a:vsn
    execute a:cmd
  endif
endfunction

execute pathogen#infect('junk/{}')

let g:ignored_dirs
\ = '\v'
\ . '^\.%(hg|git|bzr|svn)$|^_darcs$'
\ . '|%(dist|\.virthualenv|cabal-dev)$'

let g:ignored_files
\ = '\v\~|\.%'
\ . '(bak|swp|orig|test'
\ . '|jpg|png|psd|gif|zip'
\ . '|hi|p_hi|p_o|dyn_hi|dyn_o|chi|chs\.h'
\ . '|annot|cmo|cma|cmi|cmx|cmxa'
\ . '|o|lo|slo|a|la|sla|lib|so|dylib'
\ . '|exe|dll|beam|keter'
\ . ')$'
\ . '|target(\\|/).*%(class|lst|jar|xml|txt|properties)$'

let g:erlangCompletionDisplayDoc = 0
let g:erlangFoldSplitFunction = 1

let g:haddock_browser       = 'echo'
let g:haskell_conceal              = 0
let g:haskell_conceal_bad          = 0
let g:haskell_conceal_comments     = 0
let g:haskell_conceal_enumerations = 0
let g:haskell_conceal_wide         = 0
let g:haskell_cpp                  = 0
let g:haskell_ffi                  = 0
let g:haskell_haddock              = 0
let g:haskell_hsp                  = 0
let g:haskell_interpolation        = 0
let g:haskell_jmacro               = 0
let g:haskell_json                 = 0
let g:haskell_multiline_strings    = 0
let g:haskell_quasi                = 0
let g:haskell_regex                = 0
let g:haskell_rlangqq              = 0
let g:haskell_shqq                 = 0
let g:haskell_sql                  = 0
let g:haskell_tabular              = 0
let g:haskell_th                   = 0
let g:haskell_xml                  = 0

let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_underbar_completion = 1
"let g:neocomplcache_dictionary_filetype_lists = { 'default': '' }
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_skip_auto_completion_time = '3'

let g:necoghc_enable_detailed_browse = 1

let g:ghcmod_use_basedir = getcwd()
let g:ghcmod_ghc_options = []

let g:NERDTreeDirArrows = 1
let g:NERDTreeMinimalUI = 1

let g:ctrlp_cache_dir = expand('~/.local/vim/tmp/ctrlpcache')
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_map = ''
let g:ctrlp_max_height = 30
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_max_depth = 100
let g:ctrlp_max_files = 1000000
let g:ctrlp_prompt_mappings = {
  \ 'PrtHistory(-1)': [],
  \ 'PrtHistory(1)':  [],
  \ 'PrtSelectMove("j")': ['<c-n>'],
  \ 'PrtSelectMove("k")': ['<c-p>'],
  \ 'PrtBS()': ['<c-h>', '<bs>', '<c-]>'],
  \ 'PrtCurLeft()': ['<left>', '<c-^>'],
  \ 'PrtClearCache()': ['<f12>'],
  \ }
let g:ctrlp_custom_ignore = {
  \ 'dir':  g:ignored_dirs,
  \ 'file': g:ignored_files,
  \ 'link': g:ignored_files,
  \ }

imap <f1> <nop>
map <f1> <nop>
map <f2> <esc>:NERDTreeToggle<cr>
map <f3> <esc>:CtrlPBuffer<cr>
map <f4> <esc>:CtrlP<cr>
map <f12> <esc>:CtrlPClearCache<cr>

autocmd BufRead *.inf  set filetype=dosini
autocmd BufRead *.json set filetype=json

autocmd BufRead *.scons    set filetype=python
autocmd BufRead SConstruct set filetype=python
autocmd BufRead SCsub      set filetype=python

autocmd BufRead *.escript set filetype=erlang
autocmd BufRead *.md      set filetype=markdown
autocmd BufRead *.lhs     set filetype=haskell

autocmd BufNewFile,BufRead *.fs,*.fsi,*.fsx set filetype=fsharp

autocmd BufEnter * set noro

filetype plugin on
"filetype indent on
syntax on

set autoindent

set hlsearch
set ruler
set showcmd
set showmode
set noincsearch

set wildmenu
set wildmode=longest,list,full

call VsnCmd(703, 'set colorcolumn=81')
set cursorline
set modeline

"set list listchars=tab:»·,trail:·
set list listchars=tab:»\ ,trail:·
set expandtab
set copyindent
set preserveindent
set shiftwidth=4
set softtabstop=4
set tabstop=8
set fileformats=unix,dos

set backspace=indent,eol,start
set directory=~/.local/vim/tmp/swap//
set mousemodel=popup
call VsnCmd(703, 'set undodir=~/.local/vim/tmp/undo//')

set cedit=
set formatoptions=vt
set nobackup
set notildeop
set shortmess=a
set whichwrap=
set viminfo=

set maxmempattern=2000

set lz

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
  "autocmd CursorMoved * silent! exe
  "      \ printf('match IncSearch /\V\<%s\>/', escape(expand('<cword>'), '/\'))
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
call CommandCabbr('info',   'Info')
call CommandCabbr('rc',   'RefreshComplCache')

"autocmd FileType ocaml,haskell,cabal,c,cpp,cpp11,erlang
"               \,vim,python,php,markdown,sh
"               \,javascript,json
"               \ :call ConfigSourceFileBuffer()

autocmd BufRead *.tsv set noexpandtab

autocmd QuickFixCmdPost [^l]* nested Copen
autocmd QuickFixCmdPost    l* nested Lopen

if has('python')
  "Bundle 'sjl/gundo.vim'
  "Bundle 'Valloric/YouCompleteMe'

  "let g:gundo_preview_bottom = 1
  "let g:gundo_help = 0
  "map <f1> <esc>:GundoToggle<cr>
else
  "Bundle 'mbbill/undotree'
endif

"inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
"inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
"inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<TAB>"

if executable('ctags')
  "Bundle 'majutsushi/tagbar'

  let g:tagbar_compact = 1
  let g:tagbar_singleclick = 0
  let g:tagbar_iconchars = ['▶', '▼']

  map <f5> <esc>:TagbarToggle<cr>
endif

if executable('lushtags')
  let g:tagbar_type_haskell = {
    \ 'ctagsbin' : 'lushtags',
    \ 'ctagsargs' : '--ignore-parse-error --',
    \ 'kinds' : [
      \ 'm:module:0',
      \ 'e:exports:1',
      \ 'i:imports:1',
      \ 't:declarations:0',
      \ 'd:declarations:1',
      \ 'n:declarations:1',
      \ 'f:functions:0',
      \ 'c:constructors:0'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
      \ 'd' : 'data',
      \ 'n' : 'newtype',
      \ 'c' : 'constructor',
      \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
      \ 'data' : 'd',
      \ 'newtype' : 'n',
      \ 'constructor' : 'c',
      \ 'type' : 't'
    \ }
  \ }
endif

let g:solarized_bold = 0
let g:solarized_italic = 0
let g:solarized_underline = 0
let g:solarized_termcolors = 256

let g:ackprg = 'ag --nogroup --nocolor --column'

if has('gui_running')
  if has('gui_win32')
    set guifont=PragmataPro_Mono:h9,PragmataPro:h9,ProfontWindows
  elseif has('gui_macvim')
    set guifont=PragmataPro\ Mono:h12,ProFontX:h9
  else
    set guifont=PragmataPro\ Mono\ 9,PragmataPro_Mono\ 9,PragmataPro\ 9,ProfontWindows\ 9
  endif
  set guioptions-=L
  set guioptions-=T
  set guioptions-=m
  set guioptions-=r
  set guioptions-=e
  "set number
  "set linespace=1

  set background=light
  colorscheme solarized
else
  set vb t_vb=
  set t_Co=256
  set cpo-=C

  colorscheme wombat256
endif

if filereadable(expand("~/.local/vim/local.vim"))
  source ~/.local/vim/local.vim
endif

set rtp-=~/.vim/after/
set rtp+=~/.vim/after/
