{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Android
    android-tools
    android-udev-rules
  ];
}
