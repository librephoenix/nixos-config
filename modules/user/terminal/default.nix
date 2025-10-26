{ config, lib, ... }:

{
  options = {
    userSettings.terminal = lib.mkOption {
      default = "alacritty";
      description = "Default terminal";
      type = lib.types.enum [ "alacritty" "kitty" ];
    };
  };

  config = {
    userSettings.alacritty.enable = lib.mkDefault (config.userSettings.terminal == "alacritty");
    userSettings.kitty.enable = lib.mkDefault (config.userSettings.terminal == "kitty");
  };
}
