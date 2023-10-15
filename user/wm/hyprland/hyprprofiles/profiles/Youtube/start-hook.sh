#!/bin/sh

hyprctl keyword unbind SUPER,S;
hyprctl keyword bind SUPER,S,exec,container-open Tech;
emacsclient --eval '(org-roam-switch-db "Producer.p\/LibrePhoenix.p" t)'
