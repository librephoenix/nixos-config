# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixosaku"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pl_PL.UTF-8";
    LC_IDENTIFICATION = "pl_PL.UTF-8";
    LC_MEASUREMENT = "pl_PL.UTF-8";
    LC_MONETARY = "pl_PL.UTF-8";
    LC_NAME = "pl_PL.UTF-8";
    LC_NUMERIC = "pl_PL.UTF-8";
    LC_PAPER = "pl_PL.UTF-8";
    LC_TELEPHONE = "pl_PL.UTF-8";
    LC_TIME = "pl_PL.UTF-8";
  };

  # KDE Plasma 6
  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.wayland.enable = true; # enable if blackscreen with plasma6
  services.desktopManager.plasma6.enable = true;

  # KDE Plasma 5
  # services.xserver.enable = true;
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  # services.xserver.displayManager.defaultSession = "plasmawayland";
  
  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "intl";
  };

  # Configure console keymap
  console.keyMap = "us-acentos";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define an user account. Don't forget to set a password with ‘passwd’.
  users.users.akunito = {
    isNormalUser = true;
    description = "akunito";
    extraGroups = [ "networkmanager" "wheel" ];
    home = "/home/akunito";
    packages = with pkgs; [
      #kate
    ];
  };
  
  users.users.aku = {
    isNormalUser = true;
    description = "aku";
    extraGroups = [ "networkmanager" "wheel" ];
    home = "/home/aku";
    packages = with pkgs; [
      #kate
    ];
  };
  # users.users.akunito = {
  #   isNormalUser = true;
  #   description = "akunito";
  #   extraGroups = [ "networkmanager" "wheel" ];
  #   home = "/home/akunito";
  #   openssh.authorizedKeys.keys = [
  #     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM/TKh6hv6ZJl7k2rlmDPUgg1iTcFA82HSLYgV+L4m6Z diego88aku@gmail.com"  # Replace with your actual public key
  #   ];
  #   packages = with pkgs; [
  #     vivaldi
  #     # firefox
  #     # they have been added on home.nix
  #   ];
  # };

  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    git
    tldr
    atuin
    btop
    fzf
    firefox
    #cryptsetup
  ];

  # # Some programs need SUID wrappers, can be configured further or are
  # # started in user sessions.
  # # programs.mtr.enable = true;
  # # programs.gnupg.agent = {
  # #   enable = true;
  # #   enableSSHSupport = true;
  # # };

  # Enable the SSH service with enhanced security
  services.openssh = {
    enable = true;
    permitRootLogin = "no";               # Disable root login
    passwordAuthentication = false;       # Disable password authentication
    extraConfig = ''
      Port 34389                          # Use a non-default port
      ListenAddress 192.168.0.80:34389    # Bind to the new port
      ListenAddress [::]:34389
      AllowUsers akunito                  # Allow only specific user
      MaxAuthTries 3                      # Limit authentication attempts
      LoginGraceTime 30s                  # Reduce grace time
    '';
  };

  # # Ensure SSH starts at boot
  # systemd.services.sshd.wantedBy = [ "multi-user.target" ];

  # # Firewall settings
  # networking.firewall = {
  #   enable = true;
  #   allowedTCPPorts = [ 34389 ];
  #   extraCommands = ''
  #     # Default deny incoming traffic
  #     iptables -P INPUT DROP
  #     iptables -P FORWARD DROP

  #     # Default allow outgoing traffic
  #     iptables -P OUTPUT ACCEPT

  #     # Allow HTTP and HTTPS traffic
  #     iptables -A INPUT -p tcp --dport 80 -j ACCEPT
  #     iptables -A INPUT -p tcp --dport 443 -j ACCEPT

  #     # Allow SSH from specific IP addresses to port 34389
  #     iptables -A INPUT -p tcp --dport 34389 -s 192.168.1.90 -j ACCEPT
  #     iptables -A INPUT -p tcp --dport 34389 -s 192.168.1.91 -j ACCEPT

  #     # Drop other connections to port 34389
  #     iptables -A INPUT -p tcp --dport 34389 -j DROP

  #     # Ensure loopback traffic is allowed
  #     iptables -A INPUT -i lo -j ACCEPT

  #     # Allow established and related connections
  #     iptables -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
  #   '';
  # };

  # Enable Fail2Ban
  # services.fail2ban = {
  #   enable = true;
  #   filters.sshd = ''
  #     [sshd]
  #     enabled = true
  #     port = 34389
  #     logpath = /var/log/auth.log
  #     maxretry = 3
  #     bantime = 600
  #   '';
  # };


  system.stateVersion = "23.11"; # Did you read the comment?


  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
