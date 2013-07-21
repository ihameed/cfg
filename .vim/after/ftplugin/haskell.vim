let b:ghc_staticoptions = '-ignore-dot-ghci -fforce-recomp'
compiler ghc

setlocal completefunc=
setlocal omnifunc=

set cmdheight=1

map <buffer> <LocalLeader>r :GHCReload<cr>
command! -buffer -nargs=0 Type  GhcModType
command! -buffer -nargs=0 Ctype GhcModTypeClear
command! -buffer -nargs=0 Info  GhcModInfo
command! -buffer -nargs=0 RefreshComplCache NeoComplCacheCachingGhc

function! SetToCabalBuild()
  if executable('cabal-ghci') && glob("*.cabal") != ''
    let a = system( 'grep "/\* package .* \*/"  dist/build/autogen/cabal_macros.h' )
    let b = system( 'sed -e "s/\/\* /-/" -e "s/\*\///"', a )
    let pkgs = "-hide-all-packages " .  system( 'xargs echo -n', b )
    let hs = "import Distribution.Dev.Interactive\n"
    let hs .= "import Data.List\n"
    let hs .= 'main = withOpts [""] error return >>= putStr . intercalate " "'
    let opts = system( 'runhaskell', hs )
    let b:ghc_staticoptions = opts . ' ' . pkgs
  else
    let b:ghc_staticoptions = '-Wall -fno-warn-name-shadowing'
  endif
  execute 'setlocal makeprg=' . g:ghc . '\ ' . escape(b:ghc_staticoptions,' ') .'\ -e\ :q\ %'
  let b:my_changedtick -=1
endfunction
