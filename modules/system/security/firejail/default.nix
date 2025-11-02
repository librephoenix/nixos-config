{ lib, config, pkgs, ... }:

let
  cfg = config.systemSettings.security.firejail;
in {
  options = {
    systemSettings.security.firejail = {
      enable = lib.mkEnableOption "Use firejail on some apps for extra security";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ firejail ];
    programs.firejail.enable = true;
    programs.firejail.wrappedBinaries = {
      brave = {
        executable = "${pkgs.brave}/bin/brave";
        profile = "${pkgs.firejail}/etc/firejail/brave.profile";
      };
      discord = {
        executable = "${pkgs.discord}/bin/discord";
        profile = "${pkgs.firejail}/etc/firejail/discord.profile";
      };
      prismlauncher = {
        executable = "${pkgs.prismlauncher}/bin/prismlauncher";
        profile = ./firejail-profiles/prismlauncher.profile;
      };
      steam = {
        executable = "${pkgs.steam}/bin/steam";
        profile = "${pkgs.firejail}/etc/firejail/steam.profile";
      };
      steam-run = {
        executable = "${pkgs.steam}/bin/steam-run";
        profile = "${pkgs.firejail}/etc/firejail/steam.profile";
      };
    };
  };
}
