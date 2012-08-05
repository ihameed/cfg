let b:ghc_staticoptions = '-ignore-dot-ghci -fforce-recomp'
compiler ghc

setlocal completefunc=
setlocal omnifunc=

set cmdheight=1

map <buffer> <LocalLeader>r :GHCReload<cr>
command! -buffer Type :GhcModType
command! -buffer Ctype :Ghc
