{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
      # Python setup
      python310Full
      imath
      pystring
  ];
}
