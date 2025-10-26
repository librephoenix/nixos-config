{ config, lib, pkgs, inputs, osConfig, ... }:

let
  cfg = config.userSettings.stylix;
  theme = import (./. + "../../../themes"+("/"+config.userSettings.stylix.theme));
in
{
  options = {
    userSettings.stylix = {
      enable = lib.mkEnableOption "Enable stylix theming";
    };
    userSettings.stylix.theme = lib.mkOption {
      default = if (osConfig.stylix.enable) then osConfig.systemSettings.stylix.theme else "io";
      type = lib.types.enum (builtins.attrNames (lib.filterAttrs (name: type: type == "directory") (builtins.readDir ../../themes)));
      description = "Theme for stylix to use for the user. A list of themes can be found in the `themes` directory.";
    };
  };

  # for whatever reason, I can't import stylix hmModule if the nixosModule is imported
  imports = lib.optionals (!osConfig.stylix.enable) [ inputs.stylix.homeManagerModules.stylix ];

  config = lib.mkIf cfg.enable {
    stylix.enable = true;
    home.file.".currenttheme".text = config.userSettings.stylix.theme;
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
      sizes = {
        terminal = 18;
        applications = 12;
        popups = 12;
        desktop = 12;
      };
    };

    # move into alacritty config
    stylix.targets.alacritty.enable = true;
    #programs.alacritty.settings = {
    #  colors = {
    #    # TODO revisit these color mappings
    #    # these are just the default provided from stylix
    #    # but declared directly due to alacritty v3.0 breakage
    #    primary.background = "#"+config.lib.stylix.colors.base00;
    #    primary.foreground = "#"+config.lib.stylix.colors.base07;
    #    cursor.text = "#"+config.lib.stylix.colors.base00;
    #    cursor.cursor = "#"+config.lib.stylix.colors.base07;
    #    normal.black = "#"+config.lib.stylix.colors.base00;
    #    normal.red = "#"+config.lib.stylix.colors.base08;
    #    normal.green = "#"+config.lib.stylix.colors.base0B;
    #    normal.yellow = "#"+config.lib.stylix.colors.base0A;
    #    normal.blue = "#"+config.lib.stylix.colors.base0D;
    #    normal.magenta = "#"+config.lib.stylix.colors.base0E;
    #    normal.cyan = "#"+config.lib.stylix.colors.base0B;
    #    normal.white = "#"+config.lib.stylix.colors.base05;
    #    bright.black = "#"+config.lib.stylix.colors.base03;
    #    bright.red = "#"+config.lib.stylix.colors.base09;
    #    bright.green = "#"+config.lib.stylix.colors.base01;
    #    bright.yellow = "#"+config.lib.stylix.colors.base02;
    #    bright.blue = "#"+config.lib.stylix.colors.base04;
    #    bright.magenta = "#"+config.lib.stylix.colors.base06;
    #    bright.cyan = "#"+config.lib.stylix.colors.base0F;
    #    bright.white = "#"+config.lib.stylix.colors.base07;
    #  };
    #  font.size = config.stylix.fonts.sizes.terminal;
    #  font.normal.family = config.stylix.fonts.monospace.name;
    #};

    # move into kitty config
    stylix.targets.kitty.enable = true;

    stylix.targets.gtk.enable = true;
    stylix.targets.kde.enable = true;
    stylix.targets.qt.enable = true;

    home.file = {
      ".config/qt5ct/colors/oomox-current.conf".source = config.lib.stylix.colors {
        template = builtins.readFile ./oomox-current.conf.mustache;
        extension = ".conf";
      };
      ".config/Trolltech.conf".source = config.lib.stylix.colors {
        template = builtins.readFile ./Trolltech.conf.mustache;
        extension = ".conf";
      };
      ".config/kdeglobals".source = config.lib.stylix.colors {
        template = builtins.readFile ./Trolltech.conf.mustache;
        extension = "";
      };
    };
    home.packages = with pkgs; [
       kdePackages.breeze kdePackages.breeze-icons
       nerd-fonts.fira-code fira-sans twitter-color-emoji
    ];

    fonts.fontconfig.defaultFonts = {
      monospace = [ config.stylix.fonts.monospace.name ];
      sansSerif = [ config.stylix.fonts.sansSerif.name ];
      serif = [ config.stylix.fonts.serif.name ];
    };
  };
}
