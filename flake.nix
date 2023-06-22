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
    username = "emmet"; # username
    name = "Emmet"; # name/identifier
    email = "librephoenix3@pm.me"; # email (used for certain configurations)
    dotfilesDir = "~/.dotfiles"; # absolute path of the repo locally
    theme = "ayu-dark"; # selcted theme from my themes directory
    wm = "xmonad"; # Selected window manager or desktop environment
    font = "Inconsolata"; # Selected font
    fontPkg = pkgs.inconsolata; # Font package

    # set pkgs to correct type
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
          modules = [ (./. + "/profiles"+("/"+profile)+"/home.nix") ]; # load home.nix from profile
          extraSpecialArgs = {
            inherit username;
            inherit name;
            inherit hostname;
            inherit email;
            inherit dotfilesDir;
            inherit theme;
            inherit font;
            inherit fontPkg;
            inherit wm;
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
        modules = [ (./. + "/profiles"+("/"+profile)+"/configuration.nix") ]; # load configuration.nix from profile
        specialArgs = {
          inherit username;
          inherit name;
          inherit hostname;
          inherit timezone;
          inherit locale;
          inherit theme;
          inherit font;
          inherit fontPkg;
          inherit wm;
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
