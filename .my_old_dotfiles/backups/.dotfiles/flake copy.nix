{

  description = "My first flake!";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; # which is the branch. You should use the last version or the unstable
    home-manager.url = "github:nix-community/home-manager/master"; # master == unstable
    home-manager.inputs.nixpkgs.follows = "nixpkgs"; # check that versions of nixpkgs and flake? are equal
  }; # git links

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      lib = nixpkgs.lib; # we pass lib to be able to use ie: lib.nixosSystem and others
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
    nixosConfigurations = {
      nixosaga = lib.nixosSystem { # nixosaga to match the hostname
        inherit system;
        modules = [ ./configuration.nix ];
      };
      	# Enable SSH service
	      services.openssh.enable = true;
        # Define SSH config
        users.users.akunito = {
          isNormalUser = true;
          home = "/home/akunito";
          shell = pkgs.bash;
          openssh.authorizedKeys.keys = [
            "ssh-rsa "
          ];
        };
    homeConfigurations = {
      aga = home-manager.lib.homeManagerConfiguration { # aga to match the username
        inherit pkgs;
        modules = [ ./home.nix ];
      };
    };

#     # Exceptions
#     nixpkgs.config = {
#         allowUnfree = true;
#         permittedInsecurePackages = pkgs.lib.optional (pkgs.obsidian.version == "1.5.12") "electron-25.9.0";
#       };
    };
}
