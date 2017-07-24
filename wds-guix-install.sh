#!/usr/bin/env bash
# File     : wds-guix-install.sh
# Created  : <2017-07-23 Sun 00:40:54 BST>
# Modified : <2017-7-24 Mon 01:00:10 BST> sharlatan
# Author   : sharlatan
# Synopsis :

set -e

[ "$UID" -eq 0 ] || { echo "Run as root. Exit."; exit 1; }

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
REQUIRE=(dirname readlink wget gpg curl sed sort getent)

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

chk_init_sys()
{ # Return init system type name.
    if [[ $(/sbin/init --version 2>/dev/null) =~ upstart ]]; then
        _msg "init system is: upstart"
        INIT_SYS="upstart"
        return 0
    elif [[ $(systemctl) =~ -\.mount ]]; then
        _msg "init system is: systemd"
        INIT_SYS="systemd"
        return 0
    elif [[ -f /etc/init.d/cron && ! -h /etc/init.d/cron ]]; then
        _msg "init system is: sysv-init"
        INIT_SYS="sysv-init"
        return 0
    else
        _msg "init system is: NA"
        INIT_SYS="NA"
        _err "Init system could not be detected."
    fi
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

    # TODO (Oleg-170723030421): set BIN_VER to latest default
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

    # Verifecation with gpg brakes in most cases, see warnings.
    # TODO (oleg-170723024746): fine a work round the gpg warnings

    #gpg --verify "guix-binary-${bin_ver}.tar.xz.sig"
    # if [[ "$?" -eq 0 ]]; then
    #    _msg "${OK} verification completed."
    # else
    #     _err "${FAIL} could not verify."
    #     exit 1
    # fi
}

sys_creat_store()
{ # Untar and move files to creat a Guix store.
    local pkg="$1"

    _msg "--- [$FUNCNAME] ---"

    # TODO (Oleg-170723030815): Add wornings if store exists.
    _msg "unpacking archive to /var and /gnu"
    tar --warning=no-timestamp \
        -C /tmp \
        -xf "$pkg"
    # TODO (Oleg-170724000543): really want to remove existing guix dir?
    if [[ -e "/var/guix" && -e "/gnu" ]]; then
        _msg "remove old files"
         rm -r /var/guix &&
             mv /tmp/var/guix /var
         rm -r /gnu &&
             mv /tmp/gnu /
    else
        _msg "fresh install"
         mv /tmp/var/guix /var &&
             mv /tmp/gnu /
    fi

    _msg "link profile to /root/.guix-profile"
    ln -sf /var/guix/profiles/per-user/root/guix-profile \
         ~root/.guix-profile

     GUIX_PROFILE="${HOME}/.guix-profile"
     source "${GUIX_PROFILE}/etc/profile"
}

sys_creat_build_usr()
{ # Create the group and user accounts for build users.

    _msg "--- [$FUNCNAME] ---"

    if [ $(getent group guixbuild) ]; then
        _msg "goup guixbuild exists"
    else
        _msg "group guixbuild does not exist."
        groupadd --system guixbuild
    fi

    for i in $(seq -w 1 10); do
        if id "guixbuilder${i}" &>/dev/null; then
            _msg "user is already in the system, reset"
            usermod -g guixbuild -G guixbuild           \
                    -d /var/empty -s "$(which nologin)" \
                    -c "Guix build user $i"             \
                    "guixbuilder${i}";
        else
            _msg "fresh install guixbuilder${i}"
            useradd -g guixbuild -G guixbuild           \
                    -d /var/empty -s "$(which nologin)" \
                    -c "Guix build user $i" --system    \
                    "guixbuilder${i}";
        fi
    done
}

sys_guixd_enable()
{ # Run the daemon, and set it to automatically start on boot.

    local info_path
    local local_bin
    local var_guix

    _msg "--- [$FUNCNAME] ---"

    info_path="/usr/local/share/info"
    local_bin="/usr/local/bin"
    var_guix="/var/guix/profiles/per-user/root/guix-profile"

    case "$INIT_SYS" in
        upstart)
            _msg "enable Guix deamon on upstart"
             initctl reload-configuration
             cp ~root/.guix-profile/lib/upstart/system/guix-daemon.conf \
                /etc/init/
             start guix-daemon
             ;;
        systemd)
            _msg "enable Guix deamon on Systemd"
            cp ~root/.guix-profile/lib/systemd/system/guix-daemon.service \
               /etc/systemd/system/

            chmod 664 /etc/systemd/guix-daemon.service

            systemctl daemon-reload &&
                systemctl start guix-daemon &&
                systemctl enable guix-daemon
            ;;
        NA|*)
            _msg "could no ditect init system, enable manually"
            echo "~root/.guix-profile/bin/guix-daemon --build-users-group=guixbuild"
            ;;
    esac

    _msg "make the guix command available to other users on the machine"

     [ -e "$local_bin" ] || mkdir -p "$local_bin"
     ln -sf "${var_guix}/bin/guix"  "$local_bin"

     [ -e "$info_path" ] || mkdir -p "$info_path"
     for i in ${var_guix}/share/info/*; do
         ln -sf "$i" "$info_path"
     done
}

sys_hydra_enable()
{ #
    echo "in progress"
    # https://www.gnu.org/software/guix/manual/guix.html#Substitutes
}

main()
{
    printf "Start %s %sv at %s\n\n" "$CMDNAME" "$CMD_VER" "$(date)"
    chk_require "${REQUIRE[*]}"
    chk_init_sys

    ABS_PATH="$(dirname "$(readlink -f "$0")")"

    get_guix_binary_list
    wget_and_gpg "$FTP_URL" "$BIN_VER"

    sys_creat_store "guix-binary-${BIN_VER}.tar.xz"
    sys_creat_build_usr
    sys_guixd_enable

    # And test install :)
    guix package -i hello
 }

main "$@"
# End of wds-guix-install.sh
