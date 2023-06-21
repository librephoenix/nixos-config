{ config, pkgs, ... }:

{
  services.gnome = {
    gnome-keyring.enable = true;
  };
}
