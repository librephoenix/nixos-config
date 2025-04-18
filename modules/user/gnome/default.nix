{ inputs, pkgs, config, lib, ... }:

let
  cfg = config.userSettings.gnome;
in
{
  options = {
    systemSettings.gnome = {
      enable = lib.mkEnableOption "Enable gnome config";
    };
  };

  config = lib.mkIf cfg.enable {
    stylix.targets.gnome.enable = true;
    stylix.targets.gtk.enable = true;
    dconf.settings = {
      "org/gnome/settings-daemon/plugins/power" = {
        ambient-enabled = false;
      };
    };
  };
}
