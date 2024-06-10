#!/bin/sh

# Post hooks to be called after a
# configuration sync

# Mainly just to reload stylix

# xmonad
pgrep xmobar &> /dev/null && echo "Killing old xmobar instances" && echo "Running killall xmobar" && killall xmobar &> /dev/null; # xmonad will restart xmobar
pgrep xmonad &> /dev/null && echo "Recompiling xmonad" && echo "Running xmonad --recompile && xmonad --restart" && xmonad --recompile &> /dev/null && xmonad --restart &> /dev/null;
pgrep .dunst-wrapped &> /dev/null && echo "Restarting dunst" && killall .dunst-wrapped && echo "Running dunst" && dunst &> /dev/null & disown;
pgrep xmonad &> /dev/null && echo "Reapplying background from stylix via feh" && echo "Running ~/.fehbg-stylix" && ~/.fehbg-stylix &> /dev/null & disown;

# hyprland
pgrep Hyprland &> /dev/null && echo "Reloading hyprland" && hyprctl reload &> /dev/null;
pgrep .waybar-wrapped &> /dev/null && echo "Restarting waybar" && killall .waybar-wrapped && echo "Running waybar" && waybar &> /dev/null & disown;
pgrep fnott &> /dev/null && echo "Restarting fnott" && killall fnott && echo "Running fnott" && fnott &> /dev/null & disown;
pgrep hyprpaper &> /dev/null && echo "Reapplying background via hyprpaper" && killall hyprpaper && echo "Running hyprpaper" && hyprpaper &> /dev/null & disown;
pgrep nwggrid-server &> /dev/null && echo "Restarting nwggrid-server" && killall nwggrid-server && echo "Running nwggrid-wrapper" && nwggrid-wrapper &> /dev/null & disown;

# emacs
pgrep emacs &> /dev/null && echo "Reloading emacs stylix theme" && echo "Running emacsclient --no-wait --eval \"(load-theme 'doom-stylix t nil)\"" && emacsclient --no-wait --eval "(load-theme 'doom-stylix t nil)" &> /dev/null;
