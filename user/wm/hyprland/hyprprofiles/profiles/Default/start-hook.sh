#!/bin/sh

hyprctl keyword unbind SUPER,S;
hyprctl keyword bind SUPER,S,exec,qutebrowser;
emacsclient --eval '(org-roam-switch-db "Personal.p" t)'
