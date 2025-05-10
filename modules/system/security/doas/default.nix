{ config, lib, pkgs, ... }:

let
  cfg = config.systemSettings.security.doas;
  adminUsers = config.systemSettings.adminUsers;
in {
  options = {
    systemSettings.security.doas = {
      enable = lib.mkEnableOption "Replace sudo with doas";
    };
  };

  config = lib.mkIf cfg.enable {
    # Doas instead of sudo
    security.doas.enable = true;
    security.sudo.enable = false;
    security.doas.extraRules = [
      {
        users = adminUsers;
        keepEnv = true;
        persist = true;
      }
      {
        users = adminUsers;
        cmd = "phoenix";
        noPass = true;
        keepEnv = true;
      }
      {
        users = adminUsers;
        cmd = "nix";
        noPass = true;
        keepEnv = true;
      }
      {
        users = adminUsers;
        cmd = "nixos-rebuild";
        noPass = true;
        keepEnv = true;
      }
      {
        users = adminUsers;
        cmd = "nix-collect-garbage";
        noPass = true;
        keepEnv = true;
      }
    ];

    environment.systemPackages = [
      pkgs.doas-sudo-shim
    ];
  };
}
