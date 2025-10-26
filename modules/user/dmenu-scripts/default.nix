{ lib, ...}:

{
  options = {
    userSettings.dmenuScripts = {
      enable = lib.mkEnableOption "Enable collection of dmenu scripts";
      dmenuCmd = lib.mkOption {
        default = "rofi -show dmenu";
        description = "dmenu command";
        type = lib.types.str;
      };
    };
  };
}
