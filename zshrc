recursive-fetch() {
    wget -e robots=off -r --no-parent --reject "index.html*" "$@"
}

agent-select() {
    local -aU tmpdir_files
    local -aU tmp_files
    local -aU osxdir_files
    local -aU files
    local localdir="$HOME/.local/ssh-agent"
    local tmpdir="$TMPDIR"
    local osxdir="/private/tmp"
    if [ -z "$localdir" ]; then
        localdir_files=( )
    else
        localdir_files=("$localdir"/ssh-*/agent.*(N)) 2>/dev/null
    fi
    if [ -z "$tmpdir" ]; then
        tmpdir_files=( )
    else
        tmpdir_files=("$tmpdir"/ssh-*/agent.*(N)) 2>/dev/null
    fi
    tmp_files=(/tmp/ssh-*/agent.*(N)) 2>/dev/null
    if [ -z "$osxdir" ]; then
        osxdir_files=( )
    else
        osxdir_files=("$osxdir"/com.apple.launchd.*/Listeners(N)) 2>/dev/null
    fi
    files=($localdir_files $tmpdir_files $tmp_files $osxdir_files)

    local file
    integer idx=0
    integer len=${#files}
    integer active=-1

    if [[ $len -ge 1 ]]; then
        for idx in {1..$len}; do
            file=$files[$idx]
            if [[ "$SSH_AUTH_SOCK" = "$file" ]]; then
                active=$idx
            fi
            echo "$idx. agent socket at $file"
            SSH_AUTH_SOCK="$file" ssh-add -l
            echo
        done

        local next_idx=
        vared -e -p "ya[$active]: " next_idx

        if [[ ($next_idx -ge 1) && ($next_idx -le $len) ]]; then
            file=$files[$next_idx]
            export SSH_AUTH_SOCK="$file"
            echo "using agent socket at $file"
            SSH_AUTH_SOCK="$file" ssh-add -l
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
    local localdir="$HOME/.local/ssh-agent"
    local tmpdir=$TMPDIR
    if [ -z "$localdir" ]; then
        localdir_files=( )
    else
        localdir_files=("$localdir"/ssh-*/agent.*(N)) 2>/dev/null
    fi
    if [ -z "$tmpdir" ]; then
        tmpdir_files=()
    else
        tmpdir_files=("$tmpdir"/ssh-*/agent.*(N)) 2>/dev/null
    fi
    tmp_files=(/tmp/ssh-*/agent.*(N)) 2>/dev/null
    files=($localdir_files $tmpdir_files $tmp_files)

    local file
    local old_IFS
    local timeout_cmd=('timeout' '5')
    if ! type timeout > /dev/null; then
      timeout_cmd=( )
    fi
    old_IFS=$IFS
    IFS=
    for file in $files; do
        #echo -n 'probing '$file'... '
        SSH_AUTH_SOCK="$file" "$timeout_cmd[@]" ssh-add -l 2>/dev/null 1>/dev/null
        local code="$?"
        if [[ "$code" -ne 0 && "$code" -ne 1 ]]; then
            #echo "deleted: $code"
            rm -r "$(dirname "$file")"
        else
            #echo 'kept'
        fi
    done
    IFS=$old_IFS
}

agent-update-screen() {
    screen -X setenv SSH_AUTH_SOCK "$SSH_AUTH_SOCK"
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
        source "$HOME/.keychain/$HOST-sh" > /dev/null
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
}

__set_bsd_clicolor() {
    export CLICOLOR='ya'
    export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
}

__use_local_smlnj() {
    path=("$HOME/.local/sml/bin" $path)
    export SMLNJ_HOME="$HOME/.local/sml"
}

__use_local_ghc() {
    path=("$HOME/.local/programs/ghc/default/bin" $path)
}

__use_local_cabal() {
    path=("$HOME/.cabal/bin" $path)
}

__use_local_ocaml() {
    if [ -f "$HOME/.opam/opam-init/init.zsh" ]; then
        . "$HOME/.opam/opam-init/init.zsh"
    fi
}

__use_local_opam() {
    path=("$HOME/.local/programs/opam/bin" $path)
}

__use_local_rust() {
    path=("$HOME/.local/programs/rust/1.83.0/bin" "$HOME/.cargo/bin" $path)
}

__use_local_pkgsrc() {
    path=("$HOME/.local/pkg/sbin" "$HOME/.local/pkg/bin" $path)
}

__use_local_nix() {
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    export MANPATH="$MANPATH":"$HOME/.nix-profile/share/man"
}

__use_local_nix_profile() {
    if [ -d "$HOME/.nix-profile" ]; then
        export C_INCLUDE_PATH="$HOME/.nix-profile/include"
        export LIBRARY_PATH="$HOME/.nix-profile/lib"
    fi
}

__add_local_path() {
    export PATH="$HOME/.local/bin":"$PATH"
}

__disable_mintty_annoying_crap() {
    echo -n '\e[?7786l' #mousewheel -> up/down
}

__simple_prompt() {
    PS1='%n@%m:%~%(!.#.$) '
}

__fancy_prompt() {
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
    case "$OSTYPE" in
        cygwin)
            export PATH='/usr/bin:/bin:/usr/local/bin:'"$PATH"
            export SHELL='/bin/zsh'
            #export TZ='America/Los_Angeles'
            export LESSHISTFILE='-'
            alias ls='ls -F --color=auto'
            alias grep='grep --color=auto'
            __set_locale
            __add_local_path
            __disable_mintty_annoying_crap
            ;;
        linux*)
            #export TZ='America/Los_Angeles'
            alias ls='ls -F --color=auto'
            alias grep='grep --color=auto'
            __set_locale
            __add_local_path
            ;;
        interix*)
            export HOME='/home/'`whoami`
            export PATH='/usr/local/sbin:'"$PATH"
            #export TZ='America/Los_Angeles'
            alias ls='ls -F'
            __add_local_path
            ;;
        darwin*)
            alias ls='ls -F'
            alias grep='grep --color'
            export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
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
    case "$TERM" in
        cons25)
            bindkey "^?" delete-char
            ;;
        screen*)
            #stty erase "$(tput kbs)"
            ;;
    esac
    case "$OSTYPE" in
        freebsd*)
        case "$TERM" in
            screen|screen-bce|screen-256color-bce)
                tput -T "$TERM" kbs > /dev/null 2>&1
                if (( $? )) then
                    export TERM="screen-256color"
                fi
            ;;
        esac
    esac
}

__bind_keys() {
    bindkey -e
    # xterm
    bindkey "\e[Z" reverse-menu-complete # shift-tab
    bindkey "\e[3~" delete-char # delete

    # xterm, screen, linux
    bindkey "\e[5~" beginning-of-history # page up
    bindkey "\e[6~" end-of-history # page down

    # screen, vt220
    bindkey "\e[1~" beginning-of-line # home
    bindkey "\e[4~" end-of-line # end

    # xterm, screen
    bindkey "\e[H" beginning-of-line # home
    bindkey "\e[F" end-of-line # end

    # xterm "application mode"
    bindkey "\eOH" beginning-of-line # home (terminfo "khome")
    bindkey "\eOF" end-of-line # end (terminfo "kend")
    #bindkey -- "${terminfo[khome]/O/[}" beginning-of-line
    #bindkey -- "${terminfo[kend]/O/[}" end-of-line

    # rxvt
    bindkey "\e[7~" beginning-of-line # home
    bindkey "\e[8~" end-of-line # end

    # xterm
    bindkey "\e[1;5D" backward-word # ctrl-left
    bindkey "\e[1;5C" forward-word # ctrl-right
}

__update_terminal_title() {
    # chpwd()
    [[ -t 1 ]] || return
    __update_title
}

__register_update_terminal_title() {
    autoload add-zsh-hook
    add-zsh-hook precmd __update_terminal_title
}

setopt IGNORE_EOF
setopt NO_BANG_HIST
setopt HIST_IGNORE_DUPS
export HISTSIZE=10000
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
zstyle ':completion:*' accept-exact-dirs true
zstyle ':completion:*' path-completion false

listsysctls () { set -A reply $(sysctl -AN ${1%.*} 2>/dev/null) }
compctl -K listsysctls sysctl
compctl -c man
compctl -c info
compctl -c which

compdef _gnu_generic start
compctl -f start

__bind_keys
__disable_flow_control
__update_title
#__use_keychain
__os_specific
#__use_local_nix_profile
__terminal_specific
#__use_local_smlnj
#__use_local_opam
#__use_local_ocaml
#__use_local_ghc
__use_local_cabal
#__use_local_rust
__register_update_terminal_title
__fancy_prompt

agent-clean

unset HISTFILE
export DOTNET_CLI_TELEMETRY_OPTOUT="true"

if [[ -z "$DBUS_SESSION_BUS_ADDRESS" ]]; then
    export DBUS_SESSION_BUS_ADDRESS="disabled:"
fi

[[ -f "$HOME/.local/local.zsh" ]] && . "$HOME/.local/local.zsh"

path=(${(Oa)path})
typeset -U path
path=(${(Oa)path})
export PATH
