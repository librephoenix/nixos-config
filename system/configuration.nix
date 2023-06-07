# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, myTheme, myBackgroundUrl, myBackgroundSha256, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
   #   ./style/stylix.nix
    ];

  nix.nixPath = [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
                  "nixos-config=$HOME/dotfiles/system/configuration.nix"
                  "/nix/var/nix/profiles/per-user/root/channels"
                ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  
  # Doas instead of sudo
  security.doas.enable = true;
  security.sudo.enable = false;
  security.doas.extraRules = [{
    users = [ "emmet" ];
    keepEnv = true;
    persist = true;
  }];

  # Pipewire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Kernel modules
  boot.kernelModules = [ "i2c-dev" "i2c-piix4" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  networking.hostName = "snowfire"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  services.gnome = {
    gnome-keyring.enable = true;
  };

  services.upower.enable = true;

  services.dbus = {
    enable = true;
    packages = [ pkgs.dconf ];
  };

  programs.dconf = {
    enable = true;
#    packages = [ pkgs.dconf ];
  };

  # Configure X11
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
    xkbOptions = "caps:escape";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+xmonad";
      lightdm.greeters.slick.enable = true;
      lightdm.background = pkgs.fetchurl {
        url = myBackgroundUrl;
        sha256 = myBackgroundSha256;
      };
      lightdm.greeters.slick.theme.name = "Adwaita-dark";
    };
    libinput = {
      touchpad.disableWhileTyping = true;
    };
  };

  services.xserver.displayManager.sessionCommands = ''
    xset -dpms
    xset s blank
    xset r rate 350 50
    xset s 300
    ${pkgs.lightlocker}/bin/light-locker --idle-hint &
  '';

  # Bluetooth
  # hardware.bluetooth.enable = true;
  # services.blueman.enable = true;

  systemd.services.upower.enable = true;
  systemd.services.auto-cpufreq.enable = true;

  users.defaultUserShell = pkgs.zsh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.emmet = {
    isNormalUser = true;
    description = "Emmet";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
    uid = 1000;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    zsh
    auto-cpufreq
    git
    # openrgb-with-all-plugins
    (pkgs.writeScriptBin "sudo" ''exec doas "$@"'')
  ];

  # OpenRGB setup
  # services.hardware.openrgb = {
  #  enable = true;
  #  motherboard = "amd";
  #};

  environment.shells = with pkgs; [ zsh ];
  programs.zsh.enable = true;

  fonts.fonts = with pkgs; [
    # Fonts
    (nerdfonts.override { fonts = [ "Inconsolata" ]; })
    powerline
    inconsolata
    inconsolata-nerdfont
    iosevka
    font-awesome
    ubuntu_font_family
    terminus_font
    gamemode
  ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Firewall
  networking.firewall.enable = true;
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Printing
  services.printing.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.openFirewall = true;

  # OpenGL
  hardware.opengl.enable = true;

  # Feral GameMode
  programs.gamemode.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}
