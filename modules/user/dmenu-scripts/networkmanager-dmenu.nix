{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.dmenuScripts;
  dmenuCmd = cfg.dmenuCmd;
in {
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ networkmanager_dmenu ];

    home.file.".config/networkmanager-dmenu/config.ini".text = ''
      [dmenu]
      dmenu_command = ''+dmenuCmd+''

      compact = True
      wifi_chars = ▂▄▆█
      list_saved = True

      [editor]
      terminal = alacritty
      # gui_if_available = <True or False> (Default: True)
    '';
  };
}
