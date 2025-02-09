{ lib, config, pkgs, ... }:

let
  cfg = config.systemSettings.gaming;
in {

  options = {
    systemSettings.gaming = {
      enable = lib.mkEnableOption "Enable Steam and games";
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "steam" "steam-unwrapped" ];
    hardware.opengl.driSupport32Bit = true;
    programs.steam.enable = true;
    environment.systemPackages = with pkgs; [ pkgs.steam gamemode prismlauncher ];
    programs.gamemode.enable = true;
  };
}
