# THIS DOES NOT WORK YET!
{ lib, buildPythonPackage, pkgs, ... }:

let name = "Impressive";
    _name = "impressive";
    version = "0.13.2";
in
  buildPythonPackage rec {
    inherit name version;

    src = fetchTarball {
      url = "https://sourceforge.net/projects/${_name}/files/${name}/${version}/${name}-${version}.tar.gz/download";
      sha256 = "sha256:0zbkqc29mgm93mysf3y5gvkaj4xxp1jv4ix1fqrcpfx3cricrkql";
    };

    phases = "installPhase";

    pyproject = false;
    doCheck = false;

    propagatedBuildInputs = with pkgs; [
      python3Packages.pygame
      python3Packages.pyopengl
      python3Packages.pillow
      python3Packages.pygame_sdl2
      python3Packages.pygame-gui
      ffmpeg
      mplayer
      xdg-utils
    ];

    installPhase = ''
      mkdir -p $out $out/bin $out/opt $out/share $out/share/doc $out/share/man
      cp $src/impressive.py $out/opt
      chmod +x $out/opt/impressive.py
      ln -s $out/opt/impressive.py $out/bin/impressive
    '';

    meta = {
      homepage = "https://impressive.sourceforge.net/";
      description = "the Chuck Norris of presentation software";
      license = lib.licenses.gpl2Only;
      maintainers = [];
    };
  }
