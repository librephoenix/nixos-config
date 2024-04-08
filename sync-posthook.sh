#!/bin/sh

# Post hooks to be called after a
# configuration sync

# Mainly just to reload stylix

# xmonad
ps -C xmobar &> /dev/null && echo "Killing old xmobar instances" && echo "Running killall xmobar" && killall xmobar &> /dev/null; # xmonad will restart xmobar
ps -C xmonad &> /dev/null && echo "Recompiling xmonad" && echo "Running xmonad --recompile && xmonad --restart" && xmonad --recompile && xmonad --restart;
ps -C .dunst-wrapped &> /dev/null && echo "Restarting dunst" && killall .dunst-wrapped && echo "Running dunst" && dunst &> /dev/null & disown;
ps -C xmonad &> /dev/null && echo "Reapplying background from stylix via feh" && echo "Running ~/.fehbg-stylix" && ~/.fehbg-stylix;

# hyprland
ps -C Hyprland &> /dev/null && echo "Reloading hyprland" && hyprctl reload
ps -C .waybar-wrapped &> /dev/null && echo "Restarting waybar" && killall .waybar-wrapped && echo "Running waybar" && waybar &> /dev/null & disown;
ps -C fnott &> /dev/null && echo "Restarting fnott" && killall fnott && echo "Running fnott" && fnott &> /dev/null & disown;
ps -C Hyprland &> /dev/null && echo "Reapplying background from stylix via swaybg" && echo "Running ~/.swaybg-stylix" && ~/.swaybg-stylix;

# emacs
ps -C emacs &> /dev/null && echo "Reloading emacs stylix theme" && echo "Running emacsclient --no-wait --eval \"(load-theme 'doom-stylix t nil)\"" && emacsclient --no-wait --eval "(load-theme 'doom-stylix t nil)";
