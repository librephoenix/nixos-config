colorBg=$1
colorFg=$2
colorFocus=$3
colorSecondary=$4

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
alttab -w 1 -t 240x160 -i 64x64 -sc 1 -bg $colorBg -fg $colorFg -frame $colorSecondary -inact $colorFg &
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
