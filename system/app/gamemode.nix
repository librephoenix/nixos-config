{ pkgs, ... }:

{
  # Feral GameMode
  environment.systemPackages = [ pkgs.gamemode ];
  programs.gamemode.enable = true;
}
