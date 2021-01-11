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
__tacky_prompt
