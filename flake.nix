{
  description = "Flake of LibrePhoenix";

  outputs = { self, nixpkgs, nixpkgs-stable, home-manager, nix-doom-emacs, nix-straight,
              stylix, blocklist-hosts, rust-overlay, hyprland-plugins,
              eaf, eaf-browser, org-nursery, org-yaap,
              org-side-tree, org-timeblock, phscroll, ... }@inputs:
  let
    # ---- SYSTEM SETTINGS ---- #
    systemSettings = {
      system = "x86_64-linux"; # system arch
      hostname = "snowfire"; # hostname
      profile = "personal"; # select a profile defined from my profiles directory
      timezone = "America/Chicago"; # select timezone
      locale = "en_US.UTF-8"; # select locale
    };

    # ----- USER SETTINGS ----- #
    userSettings = rec {
      username = "emmet"; # username
      name = "Emmet"; # name/identifier
      email = "emmet@librephoenix.com"; # email (used for certain configurations)
      dotfilesDir = "~/.dotfiles"; # absolute path of the local repo
      theme = "uwunicorn-yt"; # selcted theme from my themes directory (./themes/)
      wm = "hyprland"; # Selected window manager or desktop environment; must select one in both ./user/wm/ and ./system/wm/
      # window manager type (hyprland or x11) translator
      wmType = if (wm == "hyprland") then "wayland" else "x11";
      browser = "qutebrowser"; # Default browser; must select one from ./user/app/browser/
      defaultRoamDir = "Personal.p"; # Default org roam directory relative to ~/Org
      term = "alacritty"; # Default terminal command;
      font = "Intel One Mono"; # Selected font
      fontPkg = pkgs.intel-one-mono; # Font package
      editor = "emacsclient"; # Default editor;
      # editor spawning translator
      # generates a command that can be used to spawn editor inside a gui
      # EDITOR and TERM session variables must be set in home.nix or other module
      # I set the session variable SPAWNEDITOR to this in my home.nix for convenience
      spawnEditor = if (editor == "emacsclient") then "emacsclient -c -a 'emacs'"
                    else (if ((editor == "vim") || (editor == "nvim") || (editor == "nano")) then "exec " + term + " -e " + editor else editor);
    };


    # create patched nixpkgs
    nixpkgs-patched = (import nixpkgs { system = systemSettings.system; }).applyPatches {
      name = "nixpkgs-patched";
      src = nixpkgs;
      patches = [
                  ./patches/emacs-no-version-check.patch
                ];
    };

    # configure pkgs
    pkgs = import nixpkgs-patched {
      system = systemSettings.system;
      config = { allowUnfree = true;
                 allowUnfreePredicate = (_: true); };
      overlays = [ rust-overlay.overlays.default ];
    };

    pkgs-stable = import nixpkgs-stable {
      system = systemSettings.system;
      config = { allowUnfree = true;
                 allowUnfreePredicate = (_: true); };
      overlays = [ rust-overlay.overlays.default ];
    };

    # configure lib
    lib = nixpkgs.lib;

    # Systems that can run tests:
     supportedSystems = [
       "aarch64-linux"
       "i686-linux"
       "x86_64-linux"
     ];

    # Function to generate a set based on supported systems:
    forAllSystems = inputs.nixpkgs.lib.genAttrs supportedSystems;

    # Attribute set of nixpkgs for each system:
    nixpkgsFor = forAllSystems (system:
      import inputs.nixpkgs { inherit system; });

  in {
    homeConfigurations = {
      user = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ (./. + "/profiles"+("/"+systemSettings.profile)+"/home.nix") # load home.nix from selected PROFILE
                    #  inputs.nix-flatpak.homeManagerModules.nix-flatpak # Declarative flatpaks
                    ];
          extraSpecialArgs = {
            # pass config variables from above
            inherit pkgs-stable;
            inherit systemSettings;
            inherit userSettings;
            inherit (inputs) nix-doom-emacs;
            inherit (inputs) eaf;
            inherit (inputs) eaf-browser;
            inherit (inputs) org-nursery;
            inherit (inputs) org-yaap;
            inherit (inputs) org-side-tree;
            inherit (inputs) org-timeblock;
            inherit (inputs) phscroll;
            #inherit (inputs) nix-flatpak;
            inherit (inputs) stylix;
            inherit (inputs) hyprland-plugins;
          };
      };
    };
    nixosConfigurations = {
      system = lib.nixosSystem {
        system = systemSettings.system;
        modules = [ (./. + "/profiles"+("/"+systemSettings.profile)+"/configuration.nix") ]; # load configuration.nix from selected PROFILE
        specialArgs = {
          # pass config variables from above
          inherit pkgs-stable;
          inherit systemSettings;
          inherit userSettings;
          inherit (inputs) stylix;
          inherit (inputs) blocklist-hosts;
        };
      };
    };

    packages = forAllSystems (system:
      let pkgs = nixpkgsFor.${system}; in
      {
        default = self.packages.${system}.install;

        install = pkgs.writeShellApplication {
          name = "install";
          text = builtins.readFile ./install.sh;
        };
      });
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "nixpkgs/nixos-23.11";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";

    nix-straight.url = "github:librephoenix/nix-straight.el/pgtk-patch";
    nix-straight.flake = false;
    nix-doom-emacs.inputs.nix-straight.follows = "nix-straight";

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
    org-yaap = {
      url = "gitlab:tygrdev/org-yaap";
      flake = false;
    };
    org-side-tree = {
      url = "github:localauthor/org-side-tree";
      flake = false;
    };
    org-timeblock = {
      url = "github:ichernyshovvv/org-timeblock";
      flake = false;
    };
    phscroll = {
      url = "github:misohena/phscroll";
      flake = false;
    };

    stylix.url = "github:danth/stylix";

    rust-overlay.url = "github:oxalica/rust-overlay";

    #nix-flatpak.url = "github:gmodena/nix-flatpak/?ref=v0.2.0";

    blocklist-hosts = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };

    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      flake = false;
    };
  };
}
