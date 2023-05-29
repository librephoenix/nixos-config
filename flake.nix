{
  description = "No, I don't understand how this works, so please don't ask";

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
  };

  outputs = { self, nixpkgs, home-manager, nix-doom-emacs, stylix, eaf, eaf-browser, rust-overlay, ... }@inputs:
  let
    system = "x86_64-linux";
    name = "emmet";
    email = "librephoenix@protonmail.com";
    dotfilesDir = "~/.dotfiles";
    theme = "ayu-dark";
    themePolarity = "dark";
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
            myThemePolarity = themePolarity;
            myBackgroundUrl = backgroundUrl;
            myBackgroundSha256 = backgroundSha256;
            inherit (inputs) eaf;
            inherit (inputs) eaf-browser;
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
        specialArgs = {
          myTheme = theme;
          myBackgroundUrl = backgroundUrl;
          myBackgroundSha256 = backgroundSha256;
        };
      };
    };
  };
}
