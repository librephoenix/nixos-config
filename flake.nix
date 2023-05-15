{
  description = "No, I don't understand how this works, so please don't ask";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, home-manager, nix-doom-emacs, stylix, ... }:
  let
    system = "x86_64-linux";
    name = "emmet";
    email = "librephoenix@protonmail.com";
    dotfilesDir = "~/dotfiles";
    theme = "tokyo-night-dark";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
    };

    lib = nixpkgs.lib;

  in {
    homeConfigurations = {
      emmet = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./user/home.nix
            nix-doom-emacs.hmModule
            stylix.homeManagerModules.stylix
          ];
          extraSpecialArgs = {
            myName = name;
            myHomeDir = "/home/"+name;
            myEmail = email;
            myDotfilesDir = dotfilesDir;
            myNixConfigurationFilePath = dotfilesDir+"/system/configuration.nix";
            myHomeManagerFilePath = dotfilesDir+"/user/home.nix";
            myTheme = theme;
          };
      };
    };
    nixosConfigurations = {
      snowfire = lib.nixosSystem {
        inherit system;
        modules = [
          ./system/configuration.nix
          # stylix.nixosModules.stylix # complains that home-manager is not defined
        ];
        # specialArgs = {
        #   myTheme = theme;
        # };
      };
    };
  };
}
