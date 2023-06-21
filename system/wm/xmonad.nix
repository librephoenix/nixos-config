{ config, pkgs, ... }:

{
  # import X11
  imports = [ ./x11.nix
              ./pipewire.nix
              ./dbus.nix
            ];

  # Setup XMonad
  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager = {
      defaultSession = "none+xmonad";
    };
  };
}
