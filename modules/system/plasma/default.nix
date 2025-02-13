{ config, lib, pkgs, ...}:

let
  cfg = config.systemSettings.plasma;
in {
  options = {
    systemSettings.plasma = {
      enable = lib.mkEnableOption "Enable plasma";
    };
  };

  config = lib.mkIf cfg.enable {
    systemSettings.tlp.enable = lib.mkForce false;
    services.xserver.enable = true;
    services.xserver = {
      layout = "us";
      xkbVariant = "";
      xkbOptions = "caps:escape";
    };
    services.xserver.displayManager.sddm.enable = true;
    services.xserver.displayManager.sddm.wayland.enable = true;
    services.xserver.desktopManager.plasma6.enable = true;
  
    services.printing.enable = true;
  
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  
    environment.systemPackages = with pkgs; [
      kdePackages.kate
      kdePackages.dolphin
    ];
  
    virtualisation.waydroid.enable = true;
    services.avahi.nssmdns4 = true;
  };
}
