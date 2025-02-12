{ config, lib, pkgs, osConfig, ... }:

let
  cfg = config.userSettings.git;
in {
  options = {
    userSettings.git = {
      enable = lib.mkEnableOption "Enable git";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.git ];
    programs.git.enable = true;
    programs.git.userName = config.userSettings.name;
    programs.git.userEmail = config.userSettings.email;
    programs.git.extraConfig = {
      init.defaultBranch = "main";
      safe.directory = [ osConfig.systemSettings.dotfilesDir
                         osConfig.systemSettings.secretsFlakeDir
                         (config.home.homeDirectory + "/.cache/nix/tarball-cache") ];
    };
  };
}
