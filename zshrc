recursive-fetch() {
    wget -e robots=off -r --no-parent --reject "index.html*" "$@"
}

agent-select() {
    local -aU tmpdir_files
    local -aU tmp_files
    local -aU files
    local tmpdir=$TMPDIR
    if [ -z "$tmpdir" ]; then
        tmpdir_files=( )
    else
        tmpdir_files=($tmpdir/ssh-*/agent.*(N)) 2>/dev/null
    fi
    tmp_files=(/tmp/ssh-*/agent.*(N)) 2>/dev/null
    files=($tmpdir_files $tmp_files)

    local file
    integer idx=0
    integer len=${#files}
    integer active=-1


    if [[ $len -ge 1 ]]; then
        for idx in {1..$len}; do
            file=$files[$idx]
            if [[ $SSH_AUTH_SOCK = $file ]]; then
                active=$idx
            fi
            echo $idx'. agent socket at '$file
            SSH_AUTH_SOCK=$file ssh-add -l
            echo
        done

        echo -n 'ya['$active']: '
        read idx

        if [[ ($idx -ge 1) && ($idx -le $len) ]]; then
            file=$files[$idx]
            export SSH_AUTH_SOCK=$file
            echo 'using agent socket at '$file
            SSH_AUTH_SOCK=$file ssh-add -l
        fi
    else
        echo 'no agent sockets found'
    fi
}

agent-relink() {
}

agent-clean() {
    local -aU tmpdir_files
    local -aU tmp_files
    local -aU files
    local tmpdir=$TMPDIR
    if [ -z "$tmpdir" ]; then
        tmpdir_files=()
    else
        tmpdir_files=($tmpdir/ssh-*/agent.*(N)) 2>/dev/null
    fi
    tmp_files=(/tmp/ssh-*/agent.*(N)) 2>/dev/null
    files=($tmpdir_files $tmp_files)

    local file
    local old_IFS
    local timeout_cmd='timeout 5'
    if ! type timeout > /dev/null; then
      timeout_cmd=''
    fi
    old_IFS=$IFS
    IFS=
    for file in $files; do
        #echo -n 'probing '$file'... '
        local cmd="SSH_AUTH_SOCK=$file $timeout_cmd ssh-add -l 2>/dev/null 1>/dev/null"
        eval $cmd
        local code=$?
        if [[ $code -ne 0 && $code -ne 1 ]]; then
            #echo 'deleted: ' $code
            rm -r $(dirname $file)
        else
            #echo 'kept'
        fi
    done
    IFS=$old_IFS
}

ghc-pkg-clean() {
    for p in `ghc-pkg check $* 2>&1  | grep problems \
            | awk '{print $6}' | sed -e 's/:$//'`
    do
        echo unregistering $p; ghc-pkg $* unregister $p
    done
}

__disable_flow_control() {
    unsetopt FLOW_CONTROL
    stty stop undef
    stty start undef
}

__use_keychain() {
    if which keychain > /dev/null 2>&1; then
        keychain -q -Q --inherit any
        source ~/.keychain/$HOST-sh > /dev/null
    fi
}

__update_title() {
    case $TERM in
        sun-cmd)
            print -Pn "\e]l%~\e\\"
            ;;
        *xterm*|rxvt*|(dt|k|E)term|screen*)
            print -Pn "\e]2;%n@%m:%~\a"
            ;;
    esac
}

__set_locale() {
    export LANG='en_US.UTF-8'
    export LC_ALL='en_US.UTF-8'
}

__set_bsd_clicolor() {
    export CLICOLOR='ya'
    export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
}

__use_local_smlnj() {
    path=($HOME/.local/sml/bin $path)
    export SMLNJ_HOME=$HOME/.local/sml
}

__use_local_ghc() {
    path=($HOME/.local/ghc/bin $path)
}

__use_local_cabal() {
    path=($HOME/.cabal/bin $path)
}

__use_local_emacs() {
    path=($HOME/.local/emacs/bin $path)
}

__use_local_ocaml() {
    if [ -f $HOME/.opam/opam-init/init.zsh ]; then
        . $HOME/.opam/opam-init/init.zsh
    fi
}

__use_local_opam() {
    path=($HOME/.local/opam/bin $path)
}

__use_local_pkgsrc() {
    path=($HOME/.local/pkg/sbin $HOME/.local/pkg/bin $path)
}

__use_local_nix() {
    source $HOME/.nix-profile/etc/profile.d/nix.sh
    export MANPATH=$MANPATH:$HOME'/.nix-profile/share/man'
}

__use_local_nix_profile() {
    if [ -d $HOME/.nix-profile ]; then
        export C_INCLUDE_PATH=$HOME/.nix-profile/include
        export LIBRARY_PATH=$HOME/.nix-profile/lib
    fi
}

__add_local_path() {
    export PATH=$HOME'/.local/bin:'$PATH
}

__disable_mintty_retardation() {
    echo -n '\e[?7786l' #mousewheel -> up/down
}

__simple_prompt() {
    PS1='%n@%m:%~%(!.#.$) '
}

__tacky_prompt() {
    magenta=$'%{\e[95m%}'
    blue=$'%{\e[94m%}'
    reset='%f'
    username='%n'
    separator='@'
    hostname='%m'
    curdir='%~'
    privind='%(!.#.$)'
    PS1=$magenta$username$separator$hostname$reset' '$blue$curdir$reset$privind' '
}

__os_specific() {
    case $OSTYPE in
        cygwin)
            export PATH='/usr/bin:/bin:/usr/local/bin:'$PATH
            export SHELL='/bin/zsh'
            export TZ='America/Los_Angeles'
            export LESSHISTFILE='-'
            alias ls='ls -F --color'
            alias grep='grep --color'
            __set_locale
            __add_local_path
            __disable_mintty_retardation
            ;;
        linux*)
            export PATH=$HOME'/.local/bin:'$PATH
            export TZ='America/Los_Angeles'
            alias ls='ls -F --color'
            alias grep='grep --color'
            __set_locale
            __add_local_path
            ;;
        interix*)
            export HOME='/home/'`whoami`
            export PATH='/usr/local/sbin:'$PATH
            export TZ='America/Los_Angeles'
            alias ls='ls -F'
            __add_local_path
            ;;
        darwin*)
            alias ls='ls -F'
            alias grep='grep --color'
            export PATH='/opt/local/bin:/opt/local/sbin:'$PATH
            export TERMINFO=/opt/local/share/terminfo
            __set_locale
            __add_local_path
            __set_bsd_clicolor
            ;;
        freebsd*)
            export XLIB_SKIP_ARGB_VISUALS=1
            alias ls='ls -F'
            alias grep='grep --color'
            __set_bsd_clicolor
            # case $TERM in
            #     xterm-256color)
            #         export TERM='xterm'
            #         ;;
            # esac
            ;;
    esac
}

__terminal_specific() {
    case $TERM in
        cons25)
            bindkey "^?" delete-char
            ;;
        screen|screen-bce|screen-256color-bce)
            if tput -T screen-256color > /dev/null 2>&1; then
                export TERM="screen-256color"
            fi
            ;;
    esac
}

__bind_keys() {
    bindkey -e
    bindkey "\e[Z" reverse-menu-complete
    bindkey "\e[3~" delete-char

    bindkey "\e[5~" beginning-of-history
    bindkey "\e[6~" end-of-history

    bindkey "\e[1~" beginning-of-line
    bindkey "\e[4~" end-of-line
    bindkey "\e[H" beginning-of-line
    bindkey "\e[F" end-of-line
    bindkey "\e[7~" beginning-of-line
    bindkey "\e[8~" end-of-line

    bindkey "\e[5C" forward-word
    bindkey "\e[5D" backward-word
    bindkey "\e\e[C" forward-word
    bindkey "\e\e[D" backward-word
    bindkey "\eOc" emacs-forward-word
    bindkey "\eOd" emacs-backward-word
}

chpwd() {
    [[ -t 1 ]] || return
    __update_title
}

setopt IGNORE_EOF
setopt NO_BANG_HIST
setopt HIST_IGNORE_DUPS
export PAGER='less'
export LESS='-SR'
export WINEDEBUG=fixme-all
export EDITOR='vim'
export SYSSCREENRC=/dev/null
alias figee=fg
alias figs=jobs
alias pdflatex='pdflatex -interaction nonstopmode'
alias xelatex='xelatex -halt-on-error -interaction nonstopmode --shell-escape'
alias mysql='mysql --sigint-ignore'
alias ocaml='rlwrap ocaml'
alias sml='rlwrap sml'
alias racket='rlwrap racket'
alias emacs-cli='emacs -nw'
alias ssh-xforward='ssh -Y -C'

autoload -Uz compinit
compinit

listsysctls () { set -A reply $(sysctl -AN ${1%.*} 2>/dev/null) }
compctl -K listsysctls sysctl
compctl -c man
compctl -c info
compctl -c which


__bind_keys
__disable_flow_control
__update_title
#__use_keychain
__os_specific
#__use_local_nix_profile
__terminal_specific
#__use_local_smlnj
__use_local_opam
__use_local_ocaml
__use_local_ghc
__use_local_cabal
__tacky_prompt

agent-clean

path=(${(Oa)path})
typeset -U path
path=(${(Oa)path})
export PATH
