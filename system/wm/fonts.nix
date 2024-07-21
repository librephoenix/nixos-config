{ pkgs, ... }:

{
  # Fonts are nice to have
  fonts.packages = with pkgs; [
    # Fonts
    nerdfonts
    lxgw-wenkai
    sarasa-gothic
    # powerline # FIXME broken by python 311 -> 312 nixpkgs update
  ];

}
