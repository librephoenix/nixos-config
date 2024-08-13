{ lib, stdenv, fetchFromGitHub, dmenuCmd ? "rofi -dmenu", ... }:

let name = "qute-containers";
    version = "unstable";
    dmenu = dmenuCmd;
in
  stdenv.mkDerivation {
    inherit name version;

    src = fetchFromGitHub {
      owner = "s-praveen-kumar";
      repo = "qute-containers";
      rev = "c6164b94104fa8565200b87bfc87a2e08ca15ac7";
      sha256 = "sha256-g684sPSEJTRSk2V8LVrQsNeRIYtaQueRpZeREWtmQKw=";
    };

    phases = "installPhase";

    postPatch = ''sed -i "s/qutebrowser/qutebrowser --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=4/g" container-open'';

    installPhase = ''
      mkdir -p $out $out/bin
      cp $src/* $out/bin
      sed -i 's/DMENU=\"rofi -dmenu\"/DMENU=\"''+dmenu+''\"/g' $out/bin/containers_config
      sed -i 's/DMENU_FLAGS//g' $out/bin/container-open
    '';

    meta = {
      homepage = "https://github.com/s-praveen-kumar/qute-containers";
      description = "Browser Containers for Qutebrowser";
      license = lib.licenses.mit;
      maintainers = [];
    };
  }
