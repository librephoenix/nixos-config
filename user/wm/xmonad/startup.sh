#!/bin/sh

trayertint=$1

nbColor=$2
nfColor=$3
sbColor=$4
sfColor=$5

themeGTKName=$6
themeAlacrittyName=$7
themeDoomEmacsName=$8

colorBgNormal=$2
colorBgBright=${27}
colorFgNormal=$3
color01Normal=$9
color01Bright=${10}
color02Normal=${11}
color02Bright=${12}
color03Normal=${13}
color03Bright=${14}
color04Normal=${15}
color04Bright=${16}
color05Normal=${17}
color05Bright=${18}
color06Normal=${19}
color06Bright=${20}
color07Normal=${21}
color07Bright=${22}
color08Normal=${23}
color08Bright=${24}
colorFocus=${25}
colorSecondary=${26}

# Startup shell script called by xmonad to start necessary programs
#
## Kill previous instances of applications (Prevents multiple instances of the following if XMonad is restarted durin the X session)
killall xmobar
killall trayer
killall nm-applet
killall nextcloud
killall xautolock
killall caffeine
killall syncthing-gtk
killall discord
killall qjoypad

# Launch necessary desktop applications
emacs --daemon &
picom --animations --animation-window-mass 1 --animation-for-open-window zoom --animation-stiffness 200 --experimental-backends && # requires picom-pijulius
xset r rate 350 50 &
setxkbmap -option caps:escape &
~/.fehbg &
twmnd &
alttab -w 1 -t 240x160 -i 64x64 -sc 1 -bg $colorBgNormal -fg $colorFgNormal -frame $colorSecondary -inact $colorFgNormal &
autokey-gtk &
##/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --height 28 --tint $trayertint --monitor "primary" &
nm-applet &
GOMAXPROCS=1 syncthing --no-browser &
rclone mount adantium-nextcloud:/ ~/Nextcloud &
syncthing-gtk -m &
protonmail-bridge --no-window
~/.local/bin/setup-external-monitor.sh &
rm -rf ~/org &
gnome-keyring-daemon --daemonize --login &
gnome-keyring-daemon --start --components=secrets &
