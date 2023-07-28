colorBg=$1
colorFg=$2
colorFocus=$3
colorSecondary=$4

# Startup shell script called by xmonad to start necessary programs
#
## Kill previous instances of applications (Prevents multiple instances of the following if XMonad is restarted durin the X session)
killall xmobar
killall nm-applet

# Launch necessary desktop applications
autorandr;
picom --animations --animation-window-mass 1 --animation-for-open-window zoom --animation-stiffness 200 --experimental-backends && # requires picom-pijulius
xset r rate 350 50 &
setxkbmap -option caps:escape &
~/.fehbg-stylix &
~/.config/xmobar/xmobar-st-check.sh &
alttab -w 1 -t 240x160 -i 64x64 -sc 1 -bg $colorBg -fg $colorFg -frame $colorSecondary -inact $colorFg &
##/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --height 28 --tint $trayertint --monitor "primary" &
nm-applet &
GOMAXPROCS=1 syncthing --no-browser &
protonmail-bridge --noninteractive &
emacs --daemon &
gnome-keyring-daemon --daemonize --login &
gnome-keyring-daemon --start --components=secrets &
