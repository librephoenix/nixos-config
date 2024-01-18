{ pkgs, ... }:

{
  home.packages = with pkgs; [
    blueman
  ];
  services = {
    blueman-applet.enable = true;
  };
}
