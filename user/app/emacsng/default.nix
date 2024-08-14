{ config, lib, pkgs, inputs, ... }:

{
  home.packages = [
    inputs.emacsng.packages.${pkgs.system}.emacsngWRPgtk
    pkgs.source-code-pro
  ];
}
