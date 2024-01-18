{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Gamedev
    godot_4
  ];
}
