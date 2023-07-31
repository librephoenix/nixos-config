# Firejail profile for prismlauncher
# Description: An Open Source Minecraft launcher that can manage multiple instances
# This file is overwritten after every install/update
# Persistent global definitions
include globals.local

ignore noexec ${HOME}

noblacklist ${HOME}/.local/share/PrismLauncher

include allow-java.inc

include disable-common.inc
include disable-devel.inc
include disable-interpreters.inc
include disable-programs.inc
include disable-shell.inc
include disable-xdg.inc

mkdir ${HOME}/.local/share/PrismLauncher
whitelist ${HOME}/.local/share/PrismLauncher
include whitelist-common.inc
include whitelist-runuser-common.inc
include whitelist-usr-share-common.inc
include whitelist-var-common.inc

caps.drop all
netfilter
nodvd
nogroups
noinput
nonewprivs
noroot
notv
nou2f
novideo
protocol unix,inet,inet6,netlink
seccomp
tracelog

disable-mnt
private-bin java,java-config,minecraft-launcher,prismlauncher
private-cache
private-dev
# If multiplayer or realms break, add 'private-etc <your-own-java-folder-from-/etc>'
# or 'ignore private-etc' to your minecraft-launcher.local.
private-tmp

dbus-system none

restrict-namespaces
