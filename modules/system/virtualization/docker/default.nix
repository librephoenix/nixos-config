{ config, lib, pkgs, ... }:

let
  cfg = config.systemSettings.virtualization.docker;
  adminUsers = config.systemSettings.adminUsers;
in {
  options = {
    systemSettings.virtualization.docker = {
      enable = lib.mkEnableOption "Enable docker";
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      enableOnBoot = true;
      autoPrune.enable = true;
    };
    users.users = builtins.listToAttrs (map (user: { name = user; value = { extraGroups = [ "docker" ];};}) adminUsers);
    environment.systemPackages = with pkgs; [
      docker
      docker-compose
      lazydocker
    ];
  };
}
