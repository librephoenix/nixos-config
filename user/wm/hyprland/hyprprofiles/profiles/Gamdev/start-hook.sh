#!/bin/sh

hyprctl keyword unbind SUPER,S;
hyprctl keyword bind SUPER,S,exec,container-open Gamedev;
emacsclient --eval '(org-roam-switch-db "Gamedev.s" t)'
