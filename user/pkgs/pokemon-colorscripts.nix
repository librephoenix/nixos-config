{ lib, stdenv, fetchFromGitLab, pkgs, ... }:

let name = "pokemon-colorscripts";
    version = "unstable";
in
  stdenv.mkDerivation {
    inherit name version;

    src = fetchFromGitLab {
      owner = "phoneybadger";
      repo = name;
      rev = "0483c85b93362637bdd0632056ff986c07f30868";
      sha256 = "sha256-rj0qKYHCu9SyNsj1PZn1g7arjcHuIDGHwubZg/yJt7A=";
    };

    installPhase = ''
      mkdir -p $out $out/bin $out/opt
      cp -rf $src/colorscripts $out/opt
      cp $src/pokemon-colorscripts.py $out/opt
      cp $src/pokemon.json $out/opt
      ln -s $out/opt/pokemon-colorscripts.py $out/bin/pokemon-colorscripts
    '';

    meta = {
      homepage = "https://github.com/Admiral-Fish/PokeFinder";
      description = "CLI utility to print out images of pokemon to terminal";
      license = lib.licenses.mit;
      maintainers = [];
    };
  }
