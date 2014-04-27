setup-x86_64.exe ^
--no-desktop ^
--quiet-mode ^
--site http://mirrors.xmission.com/cygwin/ ^
--root C:\cygwin ^
--local-package-dir C:\cygwin\usr\distfiles ^
--packages ^
curl,^
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
texlive,^
texlive-collection-fontsextra,^
texlive-collection-fontsextra-doc,^
texlive-collection-fontsrecommended,^
texlive-collection-fontsrecommended-doc,^
texlive-collection-fontutils,^
texlive-collection-fontutils-doc,^
texlive-collection-langenglish,^
texlive-collection-langfrench,^
texlive-collection-langspanish,^
texlive-collection-latex,^
texlive-collection-latex-doc,^
texlive-collection-latexextra,^
texlive-collection-latexextra-doc,^
texlive-collection-latexrecommended,^
texlive-collection-latexrecommended-doc,^
texlive-collection-matchextra-doc,^
texlive-collection-mathextra,^
texlive-collection-music,^
texlive-collection-music-doc,^
texlive-collection-xetex,^
texlive-collection-xetex-doc,^
tmux,^
vim,^
wget,^
xdpyinfo,^
xinit,^
xorg-server,^
zsh
mkdir C:\cygwin\home
C:\cygwin\bin\ash.exe /bin/rebaseall
