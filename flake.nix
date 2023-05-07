{
  description = "Snowflakes are fractals";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, home-manager, stylix }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      lib = nixpkgs.lib;
    in
    {
    nixosConfigurations.snowfire = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [ ./system/configuration.nix ];
    };

    homeConfigurations."emmet" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.packages.x86_64-linux.default;
      modules = [ stylix.homeManagerModules.stylix ./user/home.nix ];
    };

  };
}
