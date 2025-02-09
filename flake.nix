{
  description = "Flake of LibrePhoenix";

  outputs = inputs@{ self, ... }:
    let
      system = "x86_64-linux";

      # create patched nixpkgs
      nixpkgs-patched =
        (import inputs.nixpkgs { inherit system; }).applyPatches {
          name = "nixpkgs-patched";
          src = inputs.nixpkgs;
          patches = [ ];
        };

      # configure pkgs
      # use nixpkgs if running a server (homelab or worklab profile)
      # otherwise use patched nixos-unstable nixpkgs
      pkgs = import nixpkgs-patched {
        inherit system;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = (_: true);
        };
        overlays = [ inputs.rust-overlay.overlays.default inputs.emacs-overlay.overlays.default ];
      };

      pkgs-stable = import inputs.nixpkgs-stable {
        inherit system;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = (_: true);
        };
      };

      # configure lib
      lib = inputs.nixpkgs.lib;

      # create a list of all directories inside of ./hosts
      # every directory in ./hosts has config for that machine
      hosts = builtins.filter (x: x != null) (lib.mapAttrsToList (name: value: if (value == "directory") then name else null) (builtins.readDir ./hosts));

    in {
      # generate a nixos configuration for every host in ./hosts
      nixosConfigurations = builtins.listToAttrs 
        (map (host: {
          name = host;
          value = lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              # host specific config
              { config.networking.hostName = host; }
              (./hosts + "/${host}")

              # my modules
              ./modules/system

              # home manager
              inputs.home-manager.nixosModules.home-manager
              { home-manager.extraSpecialArgs = {
                  inherit pkgs;
                  inherit pkgs-stable;
                  inherit inputs;
                };
              }

              # chaos... control!
              inputs.chaotic.nixosModules.default
            ];
            specialArgs = {
              inherit pkgs-stable;
              inherit inputs;
            };
          };
        }) hosts);
    };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "nixpkgs/nixos-24.11";
    chaotic.url = "github:chaotic-cx/nyx/5071a4037c634d41a57926521fef2e179abe3bd9";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    hyprland = {
      type = "git";
      url = "https://code.hyprland.org/hyprwm/Hyprland.git";
      submodules = true;
      rev = "3fb47372b79265ebdabeeefdad10359d5b18377a"; #v0.45.0
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hyprutils.follows = "hyprutils";
    };

    hyprutils = {
      type = "git";
      url = "https://code.hyprland.org/hyprwm/hyprutils.git";
      rev = "3c895da64b0eb19870142196fa48c07090b441c4";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprlock = {
      type = "git";
      url = "https://code.hyprland.org/hyprwm/hyprlock.git";
      rev = "3d63d9b129d5def270bc8a2471347e6f97274e2b";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hyprutils.follows = "hyprutils";
    };

    stylix.url = "github:danth/stylix";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay.url = "github:oxalica/rust-overlay";

    blocklist-hosts = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };

    secrets = {
      url = "path:/etc/nixos.secrets";
    };
  };
}
