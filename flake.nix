{
  description = "Flake of LibrePhoenix";

  outputs =
    inputs@{ self, ... }:
    let
      system = "x86_64-linux";

      # create patched nixpkgs
      nixpkgs-patched = (import inputs.nixpkgs { inherit system; }).applyPatches {
        name = "nixpkgs-patched";
        src = inputs.nixpkgs;
        patches = [
          #(builtins.fetchurl {
          #  url = "https://asdf1234.patch";
          #  sha256 = "sha256:qwerty123456...";
          #})
        ];
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
        overlays = [
          inputs.rust-overlay.overlays.default
          inputs.emacs-overlay.overlays.default
          inputs.chaotic.overlays.default
        ];
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
      hosts = builtins.filter (x: x != null) (
        lib.mapAttrsToList (name: value: if (value == "directory") then name else null) (
          builtins.readDir ./hosts
        )
      );

    in
    {
      # generate a nixos configuration for every host in ./hosts
      nixosConfigurations = builtins.listToAttrs (
        map (host: {
          name = host;
          value = lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              # host specific config
              { config.networking.hostName = host; }
              (./hosts + "/${host}")
              (inputs.secrets.hostSecrets.${host})

              # my modules
              ./modules/system

              # home manager
              inputs.home-manager.nixosModules.home-manager
              {
                home-manager.extraSpecialArgs = {
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
        }) hosts
      );
    };

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "nixpkgs/nixos-25.11";
    chaotic.url = "github:chaotic-cx/nyx";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    hyprland = {
      url = "github:hyprwm/Hyprland/v0.52.2?submodules=true";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprlock = {
      url = "github:hyprwm/hyprlock/v0.9.2";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    stylix.url = "github:nix-community/stylix";

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
      url = "git+file:///etc/nixos.secrets";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
