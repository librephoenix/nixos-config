{
  description = "Flake of LibrePhoenix";

  outputs = { self, nixpkgs, home-manager, nix-doom-emacs, stylix, eaf, eaf-browser, org-nursery, blocklist-hosts, rust-overlay, ... }@inputs:
  let
    # ---- SYSTEM SETTINGS ---- #
    system = "x86_64-linux"; # system arch
    hostname = "snowfire"; # hostname
    profile = "personal"; # select a profile defined from my profiles directory
    timezone = "America/Chicago"; # select timezone
    locale = "en_US.UTF-8"; # select locale

    # ----- USER SETTINGS ----- #
    name = "emmet"; # username
    email = "librephoenix3@pm.me"; # email (used for certain configurations)
    dotfilesDir = "~/.dotfiles"; # absolute path of the repo
    theme = "ayu-dark"; # selcted theme from my themes directory
    wm = "xmonad"; # Selected window manager or desktop environment

    # --- PATH CALCULATIONS -- #
    homeNixPath = ./. + "/profiles"+("/"+profile)+"/home.nix";
    configurationNixPath = ./. + "/profiles"+("/"+profile)+"/configuration.nix";
    systemWMNixPath = ./. + "/system/wm"+("/"+wm)+".nix";
    userWMNixPath = ./. + "/user/wm"+("/"+wm+"/"+wm)+".nix";
    themePolarityPath = "/themes/"+theme+"/polarity.txt";
    themePolarity = lib.removeSuffix "\n" (builtins.readFile (./. + themePolarityPath));
    backgroundUrlPath = "/themes/"+theme+"/backgroundurl.txt";
    backgroundUrl = builtins.readFile (./. + backgroundUrlPath);
    backgroundSha256Path = "/themes/"+theme+"/backgroundsha256.txt";
    backgroundSha256 = builtins.readFile (./. + backgroundSha256Path);

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
      overlays = [ rust-overlay.overlays.default ];
    };

    lib = nixpkgs.lib;

  in {
    homeConfigurations = {
      emmet = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            homeNixPath
          ];
          extraSpecialArgs = {
            myName = name;
            myHostname = hostname;
            myHomeDir = "/home/"+name;
            myEmail = email;
            myDotfilesDir = dotfilesDir;
            myTheme = theme;
            myThemePolarity = themePolarity;
            myBackgroundUrl = backgroundUrl;
            myBackgroundSha256 = backgroundSha256;
            inherit userWMNixPath;
            inherit (inputs) nix-doom-emacs;
            inherit (inputs) stylix;
            inherit (inputs) eaf;
            inherit (inputs) eaf-browser;
            inherit (inputs) org-nursery;
          };
      };
    };
    nixosConfigurations = {
      snowfire = lib.nixosSystem {
        inherit system;
        modules = [ configurationNixPath ];
        specialArgs = {
          myName = name;
          myHostname = hostname;
          myTimezone = timezone;
          myLocale = locale;
          myTheme = theme;
          myThemePolarity = themePolarity;
          myBackgroundUrl = backgroundUrl;
          myBackgroundSha256 = backgroundSha256;
          inherit systemWMNixPath;
          inherit (inputs) stylix;
          inherit (inputs) blocklist-hosts;
        };
      };
    };
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    stylix.url = "github:danth/stylix";
    rust-overlay.url = "github:oxalica/rust-overlay";
    eaf = {
      url = "github:emacs-eaf/emacs-application-framework";
      flake = false;
    };
    eaf-browser = {
      url = "github:emacs-eaf/eaf-browser";
      flake = false;
    };
    org-nursery = {
      url = "github:chrisbarrett/nursery";
      flake = false;
    };
    blocklist-hosts = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };
  };
}
