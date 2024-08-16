{ inputs, pkgs, lib, ... }:
{
  # Import wayland config
  imports = [ ./wayland.nix
              ./pipewire.nix
              ./dbus.nix
            ];

  # Enable the KDE Plasma Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
    xkbOptions = "caps:escape";
  };

  services.xserver.excludePackages = [ pkgs.xterm ];

  services.tlp.enable = lib.mkForce false;

  environment.systemPackages = with pkgs; [
    inputs.kwin-effects-forceblur.packages.${pkgs.system}.default
    kdePackages.kscreen
    kdePackages.kirigami
    kdePackages.plasma-desktop
    kdePackages.plasma-workspace
    kdePackages.kcmutils
    qt6.qtwayland
  ];

  # Security
  security = {
    pam.services.login.enableGnomeKeyring = true;
  };

  services.gnome.gnome-keyring.enable = true;

}
