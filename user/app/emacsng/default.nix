{ config, lib, pkgs, inputs, ... }:

{
  home.packages = [
    inputs.emacsng.${pkgs.system}.emacsngWRPgtk
  ];
}
