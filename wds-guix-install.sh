#!/usr/bin/env bash
# File     : wds-guix-install.sh
# Created  : <2017-07-23 Sun 00:40:54 BST>
# Modified : <2017-7-23 Sun 00:58:20 BST> sharlatan
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
REQUIRE=(dirname readlink)

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
        { _err "No arguments provided."; return 1; }

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
#+MAIN

main()
{
    printf "Start %s %sv at %s\n\n" "$CMDNAME" "$CMD_VER" "$(date)"
    chk_require "${REQUIRE[*]}"

    ABS_PATH="$(dirname "$(readlink -f "$0")")"
    echo "$ABS_PATH"
}

main "$@"
# End of wds-guix-install.sh
