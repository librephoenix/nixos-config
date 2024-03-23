#!/bin/sh

hyprctl keyword unbind SUPER,S;
hyprctl keyword bind SUPER,S,exec,container-open Bard;
emacsclient --eval '(org-roam-switch-db "Bard.p" t)'
