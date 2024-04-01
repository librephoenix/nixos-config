{ pkgs, ... }:

{
  # OpenGL
  hardware.opengl.enable = true;
  hardware.opengl.extraPackages = with pkgs; [
    rocmPackages.clr.icd
  ];
}
