{ lib, config, pkgs, inputs, ... }:

let
  cfg = config.systemSettings.stylix;
  theme = import (./. + "../../../themes"+("/"+config.systemSettings.stylix.theme));
in
{
  options = {
    systemSettings.stylix = {
      enable = lib.mkEnableOption "Enable stylix theming";
    };
    systemSettings.stylix.theme = lib.mkOption {
      default = "orichalcum";
      type = lib.types.enum (builtins.attrNames (lib.filterAttrs (name: type: type == "directory") (builtins.readDir ../../themes)));
      description = "Theme for stylix to use system wide. A list of themes can be found in the `themes` directory.";
    };
  };

  imports = [ inputs.stylix.nixosModules.stylix ];

  config = lib.mkIf cfg.enable {
    stylix.enable = true;
    stylix.autoEnable = false;
    stylix.polarity = theme.polarity;
    stylix.image = pkgs.fetchurl {
     url = theme.backgroundUrl;
     sha256 = theme.backgroundSha256;
    };
    stylix.base16Scheme = theme;
    stylix.fonts = {
      # TODO abstract fonts into an option
      monospace = {
        name = "FiraCode Nerd Font";
        package = pkgs.nerd-fonts.fira-code;
      };
      serif = {
        name = "Fira Sans";
        package = pkgs.fira-sans;
      };
      sansSerif = {
        name = "Fira Sans";
        package = pkgs.fira-sans;
      };
      emoji = {
        name = "Twitter Color Emoji";
        package = pkgs.twitter-color-emoji;
      };
    };

    stylix.targets.console.enable = true;

    stylix.targets.chromium.enable = true;

    environment.sessionVariables = {
      #QT_QPA_PLATFORMTHEME = "qt5ct";
    };
  };
}
