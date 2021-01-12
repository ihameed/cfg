__os_specific_path() {
    case "$OSTYPE" in
        cygwin)
            export PATH='/usr/bin:/bin:/usr/local/bin:'$PATH
            __add_local_path
            ;;
        linux*)
            __add_local_path
            ;;
        interix*)
            export PATH='/usr/local/sbin:'$PATH
            __add_local_path
            ;;
        darwin*)
            export PATH='/opt/local/bin:/opt/local/sbin:'$PATH
            __add_local_path
            ;;
    esac
}

__add_local_path() {
    export PATH=$HOME'/.local/bin:'$PATH
}

__os_specific_path
. ~/.bashrc
