"set gfn=sixbyten

if has("gui_win32")
    set gfn=ProfontWindows
elseif has("gui_macvim")
    set gfn=ProFontX:h9
    set noantialias
else
    set gfn=ProfontWindows\ 9
endif
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L
set lines=100 columns=200
set number
