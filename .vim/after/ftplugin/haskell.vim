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
