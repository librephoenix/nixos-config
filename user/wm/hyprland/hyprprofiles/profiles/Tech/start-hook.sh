#!/bin/sh

hyprctl keyword unbind SUPER,S;
hyprctl keyword bind SUPER,S,exec,qutebrowser-hyprprofile;
emacsclient --eval '(org-roam-switch-db "Producer.p\/LibrePhoenix.p" t)'
