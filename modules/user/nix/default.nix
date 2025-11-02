{ pkgs, ... }:

{
  config = {
    home.stateVersion = "22.11";
    home.packages = with pkgs; [
      nil nixd
      nixdoc
    ];
  };
}
