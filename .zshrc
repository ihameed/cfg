agent-select() {
    local -aU files
    files=(/tmp/ssh-*/agent.*) 2>/dev/null

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
    local -aU files
    files=(/tmp/ssh-*/agent.*) 2>/dev/null

    local file
    local old_IFS
    integer idx=0
    integer len=${#files}
    integer ret
    old_IFS=$IFS
    IFS=
    for idx in {1..$len}; do
        file=$files[$idx]
        #echo -n 'probing '$file'... '
        SSH_AUTH_SOCK=$file ssh-add -l 2>/dev/null 1>/dev/null
        if [[ $? -ne 0 && $? -ne 1 ]]; then
            #echo 'deleted'
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
    export PATH=$HOME'/.local/sml/bin:'$PATH
    export SMLNJ_HOME=$HOME'/.local/sml'
}

__use_local_ghc() {
    export PATH=$HOME'/.local/ghc/bin:'$PATH
}

__use_local_cabal() {
    export PATH=$HOME'/.cabal/bin:'$PATH
}

__use_local_emacs() {
    export PATH=$HOME'/.local/emacs/bin:'$PATH
}

__use_local_ocaml() {
    export PATH=$HOME/.local/godi/bin:$HOME/.local/godi/sbin:$PATH
}

__use_local_cyg_gardensnake() {
    export PYTHONPATH=$HOME/.local/gardensnake/lib/python2.6/site-packages:$PATH
    export PATH=$HOME'/.local/gardensnake/bin:'$PATH
}

__add_local_path() {
    export PATH=$HOME'/.local/bin:'$PATH
}

__disable_mintty_retardation() {
    echo -n '\e[?7786l' #mousewheel -> up/down
}

__os_specific() {
    case $OSTYPE in
        cygwin)
            export PATH='/bin:/usr/bin:/usr/local/bin:'$PATH':/usr/local/texlive/2011/bin/i386-cygwin'
            export MANPATH=$MANPATH'/usr/local/texlive/2011/texmf/doc/man'
            export INFOPATH=$INFOPATH'/usr/local/texlive/2011/texmf/doc/info'
            export SHELL='/bin/zsh'
            export TZ='/usr/share/zoneinfo/America/Los_Angeles'
            export NACL_SDK_ROOT='/cygdrive/c/sdk/native_client_sdk_0_5_1052'
            alias ls='ls -F --color'
            alias grep='grep --color'
            __set_locale
            __add_local_path
            __disable_mintty_retardation
            __use_local_cyg_gardensnake
            ;;
        linux*)
            export PATH=$HOME'/.local/bin:'$PATH
            export TZ='/usr/share/zoneinfo/America/Los_Angeles'
            alias ls='ls -F --color'
            alias grep='grep --color'
            __set_locale
            __add_local_path
            ;;
        interix*)
            export HOME='/home/'`whoami`
            export PATH='/usr/local/sbin:'$PATH
            export TZ='/usr/share/zoneinfo/America/Los_Angeles'
            alias ls='ls -F'
            __add_local_path
            ;;
        darwin*)
            alias ls='ls -F'
            alias grep='grep --color'
            export PATH='/opt/local/bin:/opt/local/sbin/:'$PATH
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
PS1='%n@%m:%~%(!.#.$) '
export PAGER='less'
export LESS='-S'
export WINEDEBUG=fixme-all
export EDITOR='vim'
alias figee=fg
alias figs=jobs
alias pdflatex='pdflatex -interaction nonstopmode'
alias mysql='mysql --sigint-ignore'
alias ocaml='rlwrap ocaml'
listsysctls () { set -A reply $(sysctl -AN ${1%.*} 2>/dev/null) }
compctl -K listsysctls sysctl
compctl -c man
compctl -c info
compctl -c which

__bind_keys
__disable_flow_control
__update_title
__use_keychain
__os_specific
__terminal_specific
#__use_local_smlnj
__use_local_ghc
__use_local_cabal
__use_local_ocaml

agent-clean
