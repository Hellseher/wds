#!/usr/bin/env bash
# File     : wds-guix-install.sh
# Created  : <2017-07-23 Sun 00:40:54 BST>
# Modified : <2017-7-23 Sun 02:33:53 BST> sharlatan
# Author   : sharlatan
# Synopsis :

#set -e

# Commentary:
#

# ------------------------------------------------------------------------------
#+CHANGELOG
# 0.0.1 :: <2017-07-23 Sun 00:40:54 BST>
#       + Script initiation.
#
# ==============================================================================
# ------------------------------------------------------------------------------
#+DEFAUL_CONFIG

CMD_VER=0.0.1
CMDNAME=wds-guix-install.sh
REQUIRE=(dirname readlink wget gpg curl sed sort)

OK=$'[  \033[32;1mOK\033[0m  ]'
FAIL=$'[ \033[31;1mFAIL\033[0m ]'

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

    _msg "--- [$FUNCNAME] ---"
    [ "${#cmds}" -eq "0" ] &&
        { _err "${FAIL} No arguments provided."; return 1; }

    for c in ${cmds[@]}; do
        command -v "$c" 2>&1 >/dev/null
        [ "$?" -eq "1" ] &&
            warn+=("$c")
    done

    [ "${#warn}" -ne 0 ] &&
        { _err "${FAIL} Commands ${warn[*]} are not avialable, install them first.";
          return 1; }

    _msg "${OK} verification of required commands completed"
}

# ------------------------------------------------------------------------------
#+GLOBAL_VARIABLES
FTP_URL="ftp://alpha.gnu.org/gnu/guix"

# ------------------------------------------------------------------------------
#+MAIN

get_guix_binary_list()
{ # Scan for ftp://alpha.gnu.org/gnu/guix/ and save list of binaries
    local -a bin_ver_ls

    _msg "--- [$FUNCNAME] ---"

    bin_ver_ls=("$(curl -s ftp://alpha.gnu.org/gnu/guix/ \
        | sed -n 's/.*\(guix-binary.*xz$\)/\1/p' \
        | sed -n 's/.*\(-[0-9].*-\)/\1/p' \
        | sed 's/^-\|\.tar\.xz//g')")

    if [[ "${#bin_ver_ls}" -ne "0" ]]; then
        _msg "${OK} Guix binary releases list recived."
    else
        _err "${FAIL} Could not get Guix binary releases list."
        exit 1
    fi

    for line in ${bin_ver_ls[@]}; do
        _msg "         $line"
    done | sort -h


    while true; do
        _msg "which to install? "
        read -r BIN_VER

        if [[ " ${bin_ver_ls=[*]} " == *"$bin_name"* ]]; then
            _msg "your choise: $BIN_VER"
            break
        else
            _err "${FAIL} Not right one, try again on exit C-c:"
        fi
    done
}

wget_and_gpg()
{ # Download and verify binary package.
    local url="$1"
    local bin_ver="$2"

    _msg "--- [$FUNCNAME] ---"

    _msg "trying to download"

    wget -q "${url}/guix-binary-${bin_ver}.tar.xz" 2>&1 >/dev/null
    wget -q "${url}/guix-binary-${bin_ver}.tar.xz.sig" 2>&1 >/dev/null

    if [[ "$?" -eq 0 ]]; then
       _msg "${OK} download completed."
    else
        _err "${FAIL} could not download."
        exit 1
    fi

    gpg --verify "guix-binary-${bin_ver}.tar.xz.sig"

    if [[ "$?" -eq 0 ]]; then
       _msg "${OK} verification completed."
    else
        _err "${FAIL} could not verify."
        exit 1
    fi
}
main()
{
    printf "Start %s %sv at %s\n\n" "$CMDNAME" "$CMD_VER" "$(date)"
    chk_require "${REQUIRE[*]}"

    ABS_PATH="$(dirname "$(readlink -f "$0")")"

    get_guix_binary_list
    wget_and_gpg "$FTP_URL" "$BIN_VER"
}

main "$@"
# End of wds-guix-install.sh
