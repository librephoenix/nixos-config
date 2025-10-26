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
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "steam" "steam-unwrapped" "steam-jupiter-unwrapped" "steamdeck-hw-theme" "xow_dongle-firmware" ];
    nixpkgs.config.packageOverrides = pkgs: {
      steam = pkgs.steam.override {
        extraPkgs = pkgs: with pkgs; [
          xorg.libXcursor
          xorg.libXi
          xorg.libXinerama
          xorg.libXScrnSaver
          libpng
          libpulseaudio
          libvorbis
          stdenv.cc.cc.lib
          libkrb5
          keyutils
          gamemode
        ];
      };
    };
    hardware.opengl.driSupport32Bit = true;
    programs.steam = {
      enable = true;
      extest.enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
      extraPackages = with pkgs; [
        xorg.libXcursor
        xorg.libXi
        xorg.libXinerama
        xorg.libXScrnSaver
        libpng
        libpulseaudio
        libvorbis
        stdenv.cc.cc.lib
        libkrb5
        keyutils
      ];
    };
    environment.systemPackages = with pkgs;
      [ steam
        gamemode
        prismlauncher
        inotify-tools
        protonup-qt
        (pkgs.writeScriptBin "steamgrab" ''chown -R $(pgrep -nf steam | xargs -r ps -o uname= -p):steam /opt/Steam;'')
        (pkgs.writeScriptBin "steamos-session-select" ''
          #!/bin/sh
          steam -shutdown
        '')
      ];
    programs.gamemode.enable = true;
    programs.gamescope.enable = true;
    programs.gamescope.capSysNice = false;
    programs.steam.gamescopeSession = {
      enable = true;
      env = {
        WLR_RENDERER = "vulkan";
        DXVK_HDR = "1";
        ENABLE_GAMESCOPE_WSI = "1";
        ENABLE_HDR_WSI = "1";
        WINE_FULLSCREEN_FSR = "1";
        # Games allegedly prefer X11
        #SDL_VIDEODRIVER = "x11";
      };
      args = [
       "--xwayland-count 1"
       #"--expose-wayland"

       "-e" # Enable steam integration

       "--adaptive-sync"
       "--hdr-enabled"
       "--hdr-itm-enable"

       # External monitor
       "--fullscreen"
       "--prefer-output eDP-1"
       "--output-width 1920"
       "--output-height 1080"
       "--nested-width 1920"
       "--nested-height 1080"
       "--prefer-vk-device 1002:1638" # lspci -nn | grep VGA
      ];
    };
    chaotic.hdr.enable = true;
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
    # FIXME https://github.com/NixOS/nixpkgs/issues/378447
    hardware.xone.enable = true;
    #hardware.xpadneo.enable = true;
    boot = {
      # FIXME https://github.com/NixOS/nixpkgs/issues/378447
      extraModulePackages = with config.boot.kernelPackages; [ xone ];
      #extraModulePackages = with config.boot.kernelPackages; [ xpadneo ];
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
    networking.firewall.allowedTCPPorts = [ 24872 ];
    networking.firewall.allowedUDPPorts = [ 24872 ];
  };
}
