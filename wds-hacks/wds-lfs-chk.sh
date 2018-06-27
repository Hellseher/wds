#!/usr/bin/env bash
# File     : wds-lfs-chk.sh
# Created  : <2018-6-26 Tue 23:50:52 BST>
# Modified : <2018-6-27 Wed 01:54:43 BST> Sharlatan
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
    "find"
    "g++"
    "gawk"
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

# ------------------------------------------------------------------------------

scractch ()
{
export LC_ALL=C


if [ -h /usr/bin/yacc ]; then
  echo "/usr/bin/yacc -> `readlink -f /usr/bin/yacc`";
elif [ -x /usr/bin/yacc ]; then
  echo yacc is `/usr/bin/yacc --version | head -n1`
else
  echo "yacc not found" 
fi

bzip2 --version 2>&1 < /dev/null | head -n1 | cut -d" " -f1,6-
echo -n "Coreutils: "; chown --version | head -n1 | cut -d")" -f2

if [ -h /usr/bin/awk ]; then
  echo "/usr/bin/awk -> `readlink -f /usr/bin/awk`";
elif [ -x /usr/bin/awk ]; then
  echo awk is `/usr/bin/awk --version | head -n1`
else 
  echo "awk not found" 
fi

cat /proc/version

echo 'int main(){}' > dummy.c && g++ -o dummy dummy.c
if [ -x dummy ]
  then echo "g++ compilation OK";
  else echo "g++ compilation failed"; fi
rm -f dummy.c dummy
}

main ()
{

    printf "Start %s v%s at %s\n\n" "$CMD_NAME" "$CMD_VER" "$(date)"

    chk_term
    chk_require "${REQUIRE[*]}"
    chk_version "${REQUIRE[*]}"
    chk_link "${REQUIRE[*]}"
    chk_pkg "${REQUIRE[*]}"
}

main "$@"
# End of wds-lfs-chk.sh
