#!/bin/sh

hyprctl keyword unbind SUPER,S;
hyprctl keyword bind SUPER,S,exec,container-open Teaching;
emacsclient --eval '(org-roam-switch-db "Teaching.p" t)'
