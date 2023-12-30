# THIS DOESN'T WORK YET!!!
# I'M NOT SMART ENOUGH TO FIGURE THIS OUT XD
{ lib, stdenv, fetchgit, pkgs, ... }:

let name = "pokefinder";
    version = "4.1.2";
in
  stdenv.mkDerivation {
    inherit name version;

    src = fetchgit {
      url = "https://github.com/Admiral-Fish/${name}";
      fetchSubmodules = true;
      rev = "v${version}";
      sha256 = "sha256-ps8F6IcbCNybrZ02tbLNyB3YEvKlcYgCpv5Em7Riv+Q=";
    };

    buildInputs = with pkgs; [ qt6.full qt6.qttools ];
    nativeBuildInputs = with pkgs; [ python3 pkgs.cmake qt6.wrapQtAppsHook ];

    cmakeFlags = [
       "-DCMAKE_BUILD_TYPE=RELEASE"
       "-DCMAKE_PREFIX_PATH=${pkgs.qt6.full}"
    ];

    meta = {
      homepage = "https://github.com/Admiral-Fish/PokeFinder";
      description = "Cross platform Pokémon RNG tool";
      license = lib.licenses.gpl3;
      maintainers = [];
    };
  }
