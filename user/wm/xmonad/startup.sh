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
killall twmnd
killall trayer
killall nm-applet
killall nextcloud
killall nitrogen
killall xautolock
killall caffeine
killall syncthing-gtk
killall discord
killall qjoypad

# pre-launch configurations
# dbus-update-activation-environment --all &
# ~/.local/bin/setup-external-monitor.sh &
# picom --experimental-backends &
picom --animations --animation-window-mass 1 --animation-for-open-window zoom --animation-stiffness 200 --experimental-backends && # requires picom-pijulius
xset r rate 350 50 &
setxkbmap -option caps:escape &
# betterdiscordctl --d-install flatpak install &

# setup necessary environment variables
# export QT_QPA_PLATFORMTHEME="qt5ct" &
# export GTK_THEME=$themeGTKName

sed -i 's/background_color=.*/background_color='$nbcolor'/' ~/.config/twmn/twmn.conf &
sed -i 's/foreground_color=.*/foreground_color='$sbcolor'/' ~/.config/twmn/twmn.conf &

sed -i 's/colors: .*/colors: *'$themeAlacrittyName'/' ~/.config/alacritty/alacritty.yml &
sed -i 's/colors: .*/colors: *'$themeAlacrittyName'/' ~/.config/alacritty/alacritty.org &

sed -i "s/(setq doom-theme .*/(setq doom-theme '"$themeDoomEmacsName")/" ~/.doom.d/config.el &
sed -i "s/(setq doom-theme .*/(setq doom-theme '"$themeDoomEmacsName")/" ~/.doom.d/doom.org &
sed -i "s/(setq doom-theme .*/(setq doom-theme '"$themeDoomEmacsName")/" ~/.doom.d/doom-pub.org &

cp -f ~/.config/xmobar/base-xmobarrc ~/.config/xmobar/xmobarrc &&
sed -i "s/colorBgNormal/"$colorBgNormal"/g" ~/.config/xmobar/xmobarrc # normal background
sed -i "s/colorBgBright/"$colorBgBright"/g" ~/.config/xmobar/xmobarrc # bright background
sed -i "s/colorFgNormal/"$colorFgNormal"/g" ~/.config/xmobar/xmobarrc # normal foreground
sed -i "s/color01Normal/"$color01Normal"/g" ~/.config/xmobar/xmobarrc # normal black
sed -i "s/color01Bright/"$color01Bright"/g" ~/.config/xmobar/xmobarrc # bright black
sed -i "s/color02Normal/"$color02Normal"/g" ~/.config/xmobar/xmobarrc # normal red
sed -i "s/color02Bright/"$color02Bright"/g" ~/.config/xmobar/xmobarrc # bright red
sed -i "s/color03Normal/"$color03Normal"/g" ~/.config/xmobar/xmobarrc # normal green
sed -i "s/color03Bright/"$color03Bright"/g" ~/.config/xmobar/xmobarrc # bright green
sed -i "s/color04Normal/"$color04Normal"/g" ~/.config/xmobar/xmobarrc # normal yellow
sed -i "s/color04Bright/"$color04Bright"/g" ~/.config/xmobar/xmobarrc # bright yellow
sed -i "s/color05Normal/"$color05Normal"/g" ~/.config/xmobar/xmobarrc # normal blue
sed -i "s/color05Bright/"$color05Bright"/g" ~/.config/xmobar/xmobarrc # bright blue
sed -i "s/color06Normal/"$color06Normal"/g" ~/.config/xmobar/xmobarrc # normal magenta
sed -i "s/color06Bright/"$color06Bright"/g" ~/.config/xmobar/xmobarrc # bright magenta
sed -i "s/color07Normal/"$color07Normal"/g" ~/.config/xmobar/xmobarrc # normal cyan
sed -i "s/color07Bright/"$color07Bright"/g" ~/.config/xmobar/xmobarrc # bright cyan
sed -i "s/color08Normal/"$color08Normal"/g" ~/.config/xmobar/xmobarrc # normal white
sed -i "s/color08Bright/"$color08Bright"/g" ~/.config/xmobar/xmobarrc # bright white
sed -i "s/colorFocus/"$colorFocus"/g" ~/.config/xmobar/xmobarrc # wm focus color
sed -i "s/colorSecondary/"$colorSecondary"/g" ~/.config/xmobar/xmobarrc & # xmobar highlight color

# Launch necessary desktop applications
# emacs --daemon &
# xautolock -time 10 -locker "dm-tool switch-to-greeter & systemctl suspend" &
twmnd &
alttab -w 1 -t 240x160 -i 64x64 -sc 1 -bg $colorBgNormal -fg $colorFgNormal -frame $colorSecondary -inact $colorFgNormal &
nitrogen --restore &
autokey-gtk &
##/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --height 28 --tint $trayertint --monitor "primary" &
nm-applet &
GOMAXPROCS=1 syncthing --no-browser &
rclone mount adantium-nextcloud:/ ~/Nextcloud &
syncthing-gtk -m &
# flatpak run com.discordapp.Discord --start-minimized &
protonmail-bridge --no-window
~/.local/bin/setup-external-monitor.sh &
rm -rf ~/org &
gnome-keyring-daemon --daemonize --login &
gnome-keyring-daemon --start --components=secrets &
#back4.sh 0.04 ~/Media/Backgrounds/steampunk-city.gif &
##sleep 2 && xwinwrap -b -s -fs -st -sp -nf -ov -fdt -- mpv -wid WID --really-quiet --framedrop=vo --no-audio --panscan="1.0" --loop-file=inf --osc=no ~/Downloads/gruvbox-town-mod.gif --scale="bilinear"
