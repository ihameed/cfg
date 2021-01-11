__os_specific() {
    case "$OSTYPE" in
        cygwin)
            export PATH='/usr/bin:/bin:/usr/local/bin:'$PATH
            export SHELL='/bin/bash'
            export TZ='America/Los_Angeles'
            export LESSHISTFILE='-'
            alias ls='ls -F --color'
            alias grep='grep --color'
            __set_locale
            __add_local_path
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

__set_bsd_clicolor() {
    export CLICOLOR='ya'
    export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
}

__set_locale() {
    export LANG='en_US.UTF-8'
    export LC_ALL='en_US.UTF-8'
}

__add_local_path() {
    export PATH=$HOME'/.local/bin:'$PATH
}

__tacky_prompt() {
    magenta='\[\e[95m\]'
    blue='\[\e[94m\]'
    reset='\[\e[0m\]'
    username='\u'
    separator='@'
    hostname='\h'
    curdir='\W'
    PS1=$reset'['$magenta$username$separator$hostname$reset' '$blue$curdir$reset']$ '
}

export SYSSCREENRC=/dev/null
set +o history
__os_specific
__tacky_prompt
