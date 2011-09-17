__disable_flow_control() {
    unsetopt FLOW_CONTROL
    stty stop undef
    stty start undef
}

__use_keychain() {
    if which keychain > /dev/null 2>&1; then
        keychain -q -Q
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

__os_specific() {
    case $OSTYPE in
        cygwin)
            export PATH='/bin:/usr/bin:/usr/local/bin:'$HOME'/.local/bin:'$PATH
            export SVN_EDITOR='/usr/bin/vim'
            export LANG='en_US.UTF-8'
            export LC_ALL='en_US.UTF-8'
            export SHELL='/bin/zsh'
            export TZ='/usr/share/zoneinfo/America/Los_Angeles'
            alias ls='ls -F --color'
            alias grep='grep --color'
            ;;
        linux*)
            export PATH=$HOME'/.local/bin:'$PATH
            export SVN_EDITOR='/usr/bin/vim'
            export LANG='en_US.UTF-8'
            export LC_ALL='en_US.UTF-8'
            export TZ='/usr/share/zoneinfo/America/Los_Angeles'
            alias ls='ls -F --color'
            alias grep='grep --color'
            ;;
        interix*)
            export HOME='/home/imran'
            export PATH=$HOME'/.local/bin:/usr/local/sbin:'$PATH
            export SVN_EDITOR='/usr/bin/vim'
            export TZ='/usr/share/zoneinfo/America/Los_Angeles'
            alias ls='ls -F'
            ;;
        darwin*)
            export CLICOLOR='1'
            export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
            export SVN_EDITOR='/usr/bin/vim'
            alias ls='ls -F'
            alias grep='grep --color'
            export PATH='/opt/local/bin:/opt/local/sbin/:'$PATH':'$HOME'/.local/bin'
            export TERMINFO=/opt/local/share/terminfo
            ;;
        freebsd*)
            export CLICOLOR='1'
            export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
            export SVN_EDITOR='/usr/local/bin/vim'
            export XLIB_SKIP_ARGB_VISUALS=1
            alias ls='ls -F'
            alias grep='grep --color'
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

chpwd() {
	[[ -t 1 ]] || return
    __update_title
}

setopt IGNORE_EOF
setopt NO_BANG_HIST
PS1='%n@%m:%~%(!.#.$) '
export PATH=$HOME'/.local/sml/bin:'$PATH
export SMLNJ_HOME=$HOME'/.local/sml'
export PAGER='/usr/bin/less'
export LESS='-S'
export WINEDEBUG=fixme-all
#export GIT_SSL_NO_VERIFY=1
alias figee=fg
alias figs=jobs
alias pdflatex='pdflatex -interaction nonstopmode'
alias mysql='mysql --sigint-ignore'
bindkey "\e[3~" delete-char
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
#bindkey "^\e[D" backward-word
#bindkey "^\e[C" forward-word
listsysctls () { set -A reply $(sysctl -AN ${1%.*} 2>/dev/null) }
compctl -K listsysctls sysctl
compctl -c man
compctl -c info
compctl -c which

__disable_flow_control
__update_title
__use_keychain
__os_specific
__terminal_specific

function select-agent {
    local -a files
    local file
    integer idx=1
    integer len=0
    integer active=-1

    #fix ifs shit
    files=($(find /tmp -name ssh-\* -type d -print0 2>/dev/null |
             xargs -0 -I xxx find xxx -iname agent.\* -type s -user $(id -u) -print0 |
             sort -z))
    ((len=${#files} - 1)) #HURR
    if [[ $len -ge 1 ]]; then #HURR DURR
        for idx in {1..$len}; do
            file=$files[$idx]
            if [[ $SSH_AUTH_SOCK = $file ]]; then
                active=$idx
            fi
            echo $idx'. agent socket  at '$file
            SSH_AUTH_SOCK=$file ssh-add -l
            echo
        done

        echo -n 'ya['$active']: '
        read idx

        if [[ $idx -le $len ]]; then
            file=$files[$idx]
            export SSH_AUTH_SOCK=$file
            echo 'using agent socket at '$file
            SSH_AUTH_SOCK=$file ssh-add -l
        fi
    else
        echo 'no agent sockets found'
    fi
}
