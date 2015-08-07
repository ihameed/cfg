setup-x86_64.exe ^
--no-desktop ^
--quiet-mode ^
--site http://mirrors.xmission.com/cygwin/ ^
--root C:\cygwin ^
--local-package-dir C:\cygwin\usr\distfiles ^
--packages ^
curl,^
dos2unix,^
emacs,^
gcc-core,^
gcc-fortran,^
gcc-g++,^
git,^
make,^
mercurial,^
ncurses,^
ocaml,^
openssh,^
patch,^
screen,^
subversion,^
tmux,^
vim,^
wget,^
xdpyinfo,^
xorg-server,^
zsh
mkdir C:\cygwin\home
C:\cygwin\bin\ash.exe /bin/rebaseall
