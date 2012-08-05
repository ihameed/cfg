let b:ghc_staticoptions = '-ignore-dot-ghci -fforce-recomp'
compiler ghc
map <LocalLeader>r :GHCReload<cr>
setlocal completefunc=
setlocal omnifunc=
set omnifunc=
