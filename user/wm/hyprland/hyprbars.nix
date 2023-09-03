{ config, lib, stdenv, pkgs, hyprland-plugins, ... }:

stdenv.mkDerivation rec {
        pname = "hyprbars";
        version = "unstable";
        src = "${hyprland-plugins}/hyprbars";
        nativeBuildInputs = [ pkgs.hyprland.nativeBuildInputs ];
        buildInputs = [ pkgs.hyprland pkgs.hyprland.buildInputs ];
        meta = {
          homepage = "https://gitlab.com/phoneybadger/pokemon-colorscripts";
          description = "CLI utility to print out images of pokemon to terminal";
          license = lib.licenses.mit;
          maintainers = [];
        };
}
