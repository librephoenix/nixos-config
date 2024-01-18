{ ... }:

{
  # import X11 config
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
