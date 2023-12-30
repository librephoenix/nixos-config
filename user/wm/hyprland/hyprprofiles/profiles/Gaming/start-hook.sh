#!/bin/sh

hyprctl keyword unbind SUPER,S;
hyprctl keyword bind SUPER,S,exec,container-open Gaming;
emacsclient --eval '(org-roam-switch-db "Producer.p\/Rinias.p" t)'
