__os_specific() {
    case "$OSTYPE" in
        cygwin)
            export SHELL='/bin/bash'
            export TZ='America/Los_Angeles'
            export LESSHISTFILE='-'
            alias ls='ls -F --color'
            alias grep='grep --color'
            __set_locale
            ;;
        linux*)
            export TZ='America/Los_Angeles'
            alias ls='ls -F --color'
            alias grep='grep --color'
            __set_locale
            ;;
        interix*)
            export HOME='/home/'`whoami`
            export TZ='America/Los_Angeles'
            alias ls='ls -F'
            ;;
        darwin*)
            alias ls='ls -F'
            alias grep='grep --color'
            export TERMINFO=/opt/local/share/terminfo
            __set_locale
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

__set_bsd_clicolor() {
    export CLICOLOR='ya'
    export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
}

__set_locale() {
    export LANG='en_US.UTF-8'
    export LC_ALL='en_US.UTF-8'
}

__tacky_prompt() {
    magenta='\[\e[95m\]'
    blue='\[\e[94m\]'
    reset='\[\e[0m\]'
    username='\u'
    separator='@'
    hostname='\h'
    curdir='\w'

    xterm_title_begin='\[\e]2;'
    xterm_title_end='\a\]'
    xterm_title=$xterm_title_begin$username$separator$hostname':'$curdir$xterm_title_end
    PS1=$reset$xterm_title'['$magenta$username$separator$hostname$reset' '$blue$curdir$reset']\$ '
}

export SYSSCREENRC=/dev/null
export IGNOREEOF=10
unset HISTFILE
__os_specific
__tacky_prompt
shopt -s globstar
bind 'set show-all-if-ambiguous on' 2>/dev/null
