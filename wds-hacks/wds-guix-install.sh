#!/usr/bin/env bash
# File     : wds-guix-install.sh
# Created  : <2017-07-23 Sun 00:40:54 BST>
# Modified : <2017-8-01 Tue 23:27:57 BST> #Rλatan
# Author   : #Rλatan
# Synopsis :

set -e

[ "$UID" -eq 0 ] || { echo "Run as root. Exit."; exit 1; }

# Commentary:
#

# ------------------------------------------------------------------------------
#+CHANGELOG
# 0.0.1 :: <2017-07-23 Sun 00:40:54 BST>
#       + Script initiation.
# 0.0.2 ::
#       + add new functions: chk_sys_arch, welcome
# ==============================================================================
# ------------------------------------------------------------------------------
#+DEFAUL_CONFIG

CMD_VER=0.0.1
CMDNAME=wds-guix-install
REQUIRE=(
    "dirname"
    "readlink"
    "wget"
    "gpg"
    "curl"
    "which"
    "sed"
    "sort"
    "getent"
    "mktemp"
    "rm"
    "chmod"
    "uname"
    "groupadd"
    "tr"
)

# Conditional symafors
PAS=$'[ \033[32;1mPAS\033[0m ] '
ERR=$'[ \033[31;1mERR\033[0m ] '
INF="[ INF ] "

FTP_URL="ftp://alpha.gnu.org/gnu/guix/"
HTTS_URL="https://alpha.gnu.org/gnu/guix/"

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
        { _err "${ERR}Commands ${warn[*]} are not available, install them.";
          return 1; }

    _msg "${PAS}verification of required commands completed"
}

chk_term()
{ # Check for ANSI terminal for color printing.
    local ansi_term

    if [ -t 2 ]; then
        if [ "${TERM+set}" = 'set' ]; then
            case "$TERM" in
                xterm*|rxvt*|urxvt*|linux*|vt*|eterm*)
                    ansi_term=true
                    ;;
                *)
                    ansi_term=false
                    ERR="[ ERR ]"
                    PAS="[ PAS ]"
                    ;;
            esac
        fi
    fi
}

chk_init_sys()
{ # Return init system type name.
    if [[ $(/sbin/init --version 2>/dev/null) =~ upstart ]]; then
        _msg "${INF}init system is: upstart"
        INIT_SYS="upstart"
        return 0
    elif [[ $(systemctl) =~ -\.mount ]]; then
        _msg "${INF}init system is: systemd"
        INIT_SYS="systemd"
        return 0
    elif [[ -f /etc/init.d/cron && ! -h /etc/init.d/cron ]]; then
        _msg "${INF}init system is: sysv-init"
        INIT_SYS="sysv-init"
        return 0
    else
        _msg "${INF}init system is: NA"
        INIT_SYS="NA"
        _err "${ERR}Init system could not be detected."
    fi
}

chk_sys_arch()
{ # Check for Operating system and CPU architecture type.
    local os
    local arch

    os="$(uname -s)"
    arch="$(uname -m)"

    # GUIX v0.13.0 avialable for OS Linux only as on <Sun 30 Jul 00:23:57 BST 2017>
    # https://www.gnu.org/software/guix/manual/html_node/GNU-Distribution.html
    # https://www.gnu.org/software/guix/manual/html_node/Porting.html#Porting

    case "$arch" in
        i386 | i486 | i686 | i786 | x86)
            local arch=i686
            ;;
        x86_64 | x86-64 | x64 | amd64)
            local arch=x86_64
            ;;
        *)
            _err "${ERR}Unsupported CPU type: ${arch}"
            exit 1
    esac

    case "$os" in
        Linux | linux)
            local os=linux
            ;;
        *)
            _err "${ERR}Your Operation system is unsuported yet."
    esac

    ARCH_OS="${arch}-${os}"
}

# ------------------------------------------------------------------------------
#+MAIN

guix_get_bin_list()
{ # Scan project FTP and save list of binaries
    local ftp_url="$1"
    local -a bin_ver_ls
    local latest_ver
    local default_ver

    _msg "--- [ $FUNCNAME ] ---"

    # Filter only version and architecture
    bin_ver_ls=("$(curl -s "$ftp_url" \
        | sed -n 's/.*\(guix-binary.*xz$\)/\1/p' \
        | sed -n 's/.*\(-[0-9].*-\)/\1/p' \
        | sed 's/^-\|\.tar\.xz//g')")

    latest_ver="$(echo "$bin_ver_ls" \
                       | grep -oP "([0-9]{1,2}\.){2}[0-9]{1,2}" \
                       | sort -Vu \
                       | tail -n1)"

    default_ver="guix-binary-${latest_ver}.${ARCH_OS}"

    if [[ "${#bin_ver_ls}" -ne "0" ]]; then
        _msg "${PAS}Guix binary releases list received."
        _msg "${INF}recommended for your system: ${default_ver}"
    else
        _err "${ERR}Could not get Guix binary releases list."
        exit 1
    fi

    # for line in ${bin_ver_ls[@]}; do
    #     _msg "${INF}$line"
    # done | sort -h

    # while true; do
    #     _msg "which to install? (${default_ver})"
    #     read -r BIN_VER
    #     if [[ -z "$BIN_VER" ]]; then
    #         BIN_VER="$default_ver"
    #         _msg "${PAS}default has been chousen: ${default_ver}"
    #         break
    #     elif [[ " ${bin_ver_ls=[*]} " == *"$bin_name"* ]]; then
    #         _msg "${PAS}your choise: $BIN_VER"
    #         break
    #     else
    #         _err "${ERR} Not right one, try again or exit C-c:"
    #     fi
    #     done

    # Use default to download acroding to the curled list and local ARCH-OS.
    BIN_VER="$default_ver"
}

guix_get_bin()
{ # Download and verify binary package.
    local url="$1"
    local bin_ver="$2"
    local dl_path="$3"

    _msg "--- [ $FUNCNAME ] ---"

    _msg "${INF}trying to download"

#    rm "${dl_path}/*" &&
        _msg "${INF}flush ${dl_path}"

    wget -q -P "$dl_path" "${url}/${bin_ver}.tar.xz" &>/dev/null
    wget -q -P "$dl_path" "${url}/${bin_ver}.tar.xz.sig" &>/dev/null

    if [[ "$?" -eq 0 ]]; then
       _msg "${PAS}download completed."
    else
        _err "${ERR}could not download."
        exit 1
    fi

    # Verifecation with gpg brakes in most cases, see warnings.
    # TODO (oleg-170723024746): fine a work round the gpg warnings

    #gpg --verify "guix-binary-${bin_ver}.tar.xz.sig"
    # if [[ "$?" -eq 0 ]]; then
    #    _msg "${PAS} verification completed."
    # else
    #     _err "${ERR} could not verify."
    #     exit 1
    # fi
}

sys_creat_store()
{ # Untar and move files to creat a Guix store.
    local pkg="$1"
    local tmp_path="$2"

    _msg "--- [ $FUNCNAME ] ---"

    # TODO (Oleg-170723030815): Add wornings if store exists.
    cd "$tmp_path"
    tar --warning=no-timestamp \
        --extract \
        --file "$pkg" &&
    _msg "${PAS}unpacked archive"

    # TODO (Oleg-170724000543): really want to remove existing guix dir?
    # TODO (Oleg-170731193624): hashcheck and copy only not presented?
    if [[ -e "/var/guix" || -e "/gnu" ]]; then
        _msg "${INF}Guix was previosly installed, updating files."
        cp -R "${tmp_path}/var" /var/
        cp -R "${tmp_path}/gnu" /
    else
        _msg "${INF}Guix is installing for the first time, create structure"
        mv "${tmp_path}/var/guix" /var/
        mv "${tmp_path}/gnu" /
    fi

    ln -sf /var/guix/profiles/per-user/root/guix-profile \
         ~root/.guix-profile &&
    _msg "${PAS}main profile linked to /root/.guix-profile"

     GUIX_PROFILE="${HOME}/.guix-profile"
     source "${GUIX_PROFILE}/etc/profile"
}

sys_creat_build_usr()
{ # Create the group and user accounts for build users.

    _msg "--- [ $FUNCNAME ] ---"

    if [ $(getent group guixbuild) ]; then
        _msg "${INF}goup guixbuild exists"
    else
        groupadd --system guixbuild
        _msg "${PAS}group <guixbuild> created"
    fi

    for i in $(seq -w 1 10); do
        if id "guixbuilder${i}" &>/dev/null; then
            _msg "${INF}user is already in the system, reset"
            usermod -g guixbuild -G guixbuild           \
                    -d /var/empty -s "$(which nologin)" \
                    -c "Guix build user $i"             \
                    "guixbuilder${i}";
        else
            useradd -g guixbuild -G guixbuild           \
                    -d /var/empty -s "$(which nologin)" \
                    -c "Guix build user $i" --system    \
                    "guixbuilder${i}";
            _msg "${PAS}user added <guixbuilder${i}>"
        fi
    done
}

sys_guixd_enable()
{ # Run the daemon, and set it to automatically start on boot.

    local info_path
    local local_bin
    local var_guix

    _msg "--- [ $FUNCNAME ] ---"

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
            { cp ~root/.guix-profile/lib/systemd/system/guix-daemon.service \
               /etc/systemd/system/;
            chmod 664 /etc/systemd/system/guix-daemon.service;
            systemctl daemon-reload &&
                systemctl start guix-daemon &&
                systemctl enable guix-daemon; } &&
                _msg "${PAS}enabled Guix deamon on Systemd"
            ;;
        NA|*)
            _msg "${ERR}could no detect init system, enable manually"
            echo "~root/.guix-profile/bin/guix-daemon --build-users-group=guixbuild"
            ;;
    esac

    _msg "${INF}make the guix command available to other users on the machine"

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

welcome()
{
 cat << "EOF"
   _____ _   _ _    _    _____       _
  / ____| \ | | |  | |  / ____|     (_)
 | |  __|  \| | |  | | | |  __ _   _ ___  _
 | | |_ | . ` | |  | | | | |_ | | | | \ \/ /
 | |__| | |\  | |__| | | |__| | |_| | |>  <
  \_____|_| \_|\____/   \_____|\__,_|_/_/\_\

This set up install GNU Guix on your system

(c) https://www.gnu.org/software/guix/
EOF
 echo "Procede?..."
 read -r  ANSWER
}

main()
{
    local abs_path
    local tmp_path
    welcome

    printf "Start %s v%s at %s\n\n" "$CMDNAME" "$CMD_VER" "$(date)"

    chk_term
    chk_require "${REQUIRE[*]}"
    chk_init_sys
    chk_sys_arch

    _msg "${INF}system is ${ARCH_OS}"

    abs_path="$(dirname "$(readlink -f "$0")")"
    tmp_path="$(mktemp -t -d guix.XXX)"

    guix_get_bin_list "${FTP_URL}"
    guix_get_bin "${FTP_URL}" "${BIN_VER}" "$tmp_path"

    sys_creat_store "${BIN_VER}.tar.xz" "${tmp_path}"
    sys_creat_build_usr
    sys_guixd_enable

    # And test install :)
    guix package -i hello

    rm -r "${tmp_path}" &&
        _msg "${INF}clean up ${tmp_path}"
 }

main "$@"
# End of wds-guix-install.sh
