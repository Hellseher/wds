#!/usr/bin/env bash
# File     : wds-lfs-chk.sh
# Created  : <2018-6-26 Tue 23:50:52 BST>
# Modified : <2018-7-07 Sat 01:42:50 BST> Sharlatan
# Author   : sharlatan
# Synopsis : <Check all requirements for the host to buld LFS>

set -e

# use it for debug
#set -x

# ------------------------------------------------------------------------------
#+CHANGELOG
# 0.0.1 :: <2018-6-26 Tue 23:50:52 BST>
#       + Iniit
# ==============================================================================
# ------------------------------------------------------------------------------
#+DEFAUL_CONFIG

CMD_VER=0.1.0
CMDNAME=wds-lfs-chk.sh
REQUIRE=(
    "bison"
    "cat"
    "chown"
    "cut"
    "diff"
    "md5sum"
    "wget"
    "mkdir"
    "mount"
    "find"
    "g++"
    "gawk"
    "mkfs"
    "gcc"
    "bash"
    "grep"
    "gzip"
    "sh"
    "head"
    "ld"
    "ldd"
    "m4"
    "tar"
    "make"
    "makeinfo"
    "patch"
    "perl"
    "readlink"
    "sed"
    "tar"
    "xz"
    "yacc"
    )


# Conditional symafors
PAS=$'[ \033[32;1mPAS\033[0m ] '
ERR=$'[ \033[31;1mERR\033[0m ] '
INF="[ INF ] "

# ------------------------------------------------------------------------------
#+UTILITIES_FUNCTIONS

_err()
{ # All errors go to stder.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_msg()
{ # Default message fro stdout.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

chk_require()
{ # Check that every required command is available.
    declare -a cmds
    declare -a warn

    cmds=(${1})

    _msg "--- [ $FUNCNAME ] ---"
    [ "${#cmds}" -eq "0" ] &&
        { _err "${ERR}No arguments provided."; return 1; }

    for c in ${cmds[@]}; do
        command -v "$c" &>/dev/null
        [ "$?" -eq "1" ] &&
            warn+=("$c")
    done

    [ "${#warn}" -ne 0 ] &&
        { _err "${ERR}Commands <${warn[*]}> are not available, install them.";
          return 1; }

    _msg "${PAS}verification of required commands completed"
}

chk_term()
{ # Check for ANSI terminal for color printing.
    local ansi_term

    _msg "--- [ $FUNCNAME ] ---"
    if [ -t 2 ]; then
        _msg "${INF}terminal is <${TERM}>"

        if [ "${TERM+set}" = 'set' ]; then
            case "$TERM" in
                xterm*|rxvt*|urxvt*|linux*|vt*|eterm*)
                    ANSI_TERM=true
                    ;;
                *)
                    ANSI_TERM=false
                    ERR="[ ERR ] "
                    PAS="[ PAS ] "
                    ;;
            esac
        fi
    fi
}

chk_version ()
{ # Bullet prove checking of cmd version.

    declare -a cmds
    declare -a cmd_ver
    declare -a ver_re

    cmds=(${1})
    ver_re="\b(\d+\.)?(\d+\.)?(\*|\d+)"

    _msg "--- [ $FUNCNAME ] ---"
    [ "${#cmds}" -eq "0" ] &&
        { _err "${ERR}No arguments provided."; return 1; }

    for c in ${cmds[@]}; do
        if eval "${c} -V:version" &>/dev/null; then
            cmd_ver="$(eval ${c} -V:version | grep -m1 -oP "$ver_re" | head -n1)"
            _msg "${c} ${cmd_ver}"
        elif eval "${c} --version" &>/dev/null; then
            cmd_ver="$(eval ${c} --version | grep -m1 -oP "$ver_re" | head -n1)"
            _msg "${c} ${cmd_ver}"
        elif eval "${c} -V" &>/dev/null; then
            cmd_ver="$(eval ${c} -V | grep -m1 -oP "$ver_re" | head -n1)"
            _msg "${c} ${cmd_ver}"
        elif eval "${c} --help" &>/dev/null; then
            cmd_ver="$(eval ${c} --help | grep -m1 -oP "$ver_re" | head -n1)"
            _msg "${c} ${cmd_ver}"
        elif eval "${c} -version" &>/dev/null; then
            cmd_ver="$(eval ${c} -version &>1 | grep -m1 -oP "$ver_re" | head -n1)"
            _msg "${c} ${cmd_ver}"
        fi
    done
}

chk_link ()
{ # Check if command is symlink to actual one.

    declare -a cmds
    declare -a cmd_path

    cmds=(${1})

    _msg "--- [ $FUNCNAME ] ---"
    [ "${#cmds}" -eq "0" ] &&
        { _err "${ERR}No arguments provided."; return 1; }

    for c in ${cmds[@]}; do
        cmd_path=$(command -v "$c")

        if [[ -h "$cmd_path" ]]; then
            _msg "${c} -> ${cmd_path} -> $(readlink -f "${cmd_path}")"
        fi
    done
}

chk_pkg ()
{ # Check wich package command belong to.

    declare -a cmds

    cmds=(${1})

    _msg "--- [ $FUNCNAME ] ---"
    [ "${#cmds}" -eq "0" ] &&
        { _err "${ERR}No arguments provided."; return 1; }
    for c in ${cmds[@]}; do
        if command -v rpm &>/dev/null; then
            _msg "${c} -> $(rpm -qf $(which "$c"))"
        elif command -v dpkg-query &>/dev/null; then
            dpkg-query -S $(which "$c")
        fi
    done
}

chk_build_sys ()
{

    _msg "--- [ $FUNCNAME ] ---"

    _msg "your dinamik linker is:"
    echo
    readelf -l /bin/ls | grep interpreter
    echo
    _msg "linker search order PATH"
    echo
    ld --verbose | grep "SEARCH"
    echo

    echo 'int main(){}' > dummy.c && g++ -o dummy dummy.c
    if [ -x dummy ]; then
        _msg "${PAS}g++ compilation";
    else
        _msg "${ERR}g++ compilation"
    fi
    rm -f dummy.c dummy

    echo 'int main(){}' > dummy.c && gcc -o dummy dummy.c
    if [ -x dummy ]; then
        _msg "${PAS}gcc compilation";
    else
        _msg "${ERR}gcc compilation"
    fi
    rm -f dummy.c dummy
}

# ------------------------------------------------------------------------------
recomendations ()
{
    cat <<EOF

    Your host system should have the following software with the minimum
    versions indicated.


    Bash-3.2 (/bin/sh should be a symbolic or hard link to bash)
    Binutils-2.17 (Versions greater than 2.30 are not recommended as they have not been tested)
    Bison-2.3 (/usr/bin/yacc should be a link to bison or small script that executes bison)
    Bzip2-1.0.4
    Coreutils-6.9
    Diffutils-2.8.1
    Findutils-4.2.31
    Gawk-4.0.1 (/usr/bin/awk should be a link to gawk)
    GCC-4.7
    Glibc-2.11
    Grep-2.5.1a
    Gzip-1.3.12
    Linux Kernel-3.2
    M4-1.4.10
    Make-3.81
    Patch-2.5.4
    Perl-5.8.8
    Sed-4.1.5
    Tar-1.22
    Texinfo-4.7
    Xz-5.0.0

EOF

}

main ()
{

    printf "Start %s v%s at %s\n\n" "$CMD_NAME" "$CMD_VER" "$(date)"

    chk_term
    chk_require "${REQUIRE[*]}" || exit 1
    chk_version "${REQUIRE[*]}"
    chk_link "${REQUIRE[*]}"
    chk_pkg "${REQUIRE[*]}"

    chk_build_sys

    _msg "${INF}$(cat /proc/version)"

    recomendations
}

main "$@"
# End of wds-lfs-chk.sh
