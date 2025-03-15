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
    environment.systemPackages = with pkgs;
      [ steam
        gamemode
        prismlauncher
        inotify-tools
        protonup-qt
        (pkgs.writeScriptBin "steamgrab" ''chown -R $(pgrep -nf steam | xargs -r ps -o uname= -p):steam /opt/Steam;'')
      ];
    programs.gamemode.enable = true;
    programs.gamescope.enable = true;
    programs.gamescope.caSysNice = true;
    programs.steam.gamescopeSession.enable = true;
    systemSettings.bluetooth.enable = true;
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings.General = {
        experimental = true;
        Privacy = "device";
        JustWorksRepairing = "always";
        Class = "0x000100";
        FastConnectable = true;
      };
    };
    hardware.xpadneo.enable = true;
    boot = {
      extraModulePackages = with config.boot.kernelPackages; [ xpadneo ];
      extraModprobeConfig = ''
        options bluetooth disable_ertm=Y
      '';
    };
    users.groups = {
      steam = {
        members = config.systemSettings.users;
      };
    };
    systemd.services.steamshare = {
      enable = true;
      unitConfig = {
        Type = "exec";
      };
      serviceConfig = {
        ExecStart = [ ''/run/current-system/sw/bin/chmod -R 777 /opt/Steam;'' ''/run/current-system/sw/bin/inotifywait -mr -e close_write,create,moved_to,modify /opt/Steam | while /run/current-system/sw/bin/read path file; do /run/current-system/sw/bin/chmod 777 $path/$file; done;'' ];
      };
      wantedBy = [ "graphical.target" ];
    };
    services.cron = {
      enable = true;
      systemCronJobs = [
        "*/1 * * * * steamgrab"
      ];
    };
  };
}
