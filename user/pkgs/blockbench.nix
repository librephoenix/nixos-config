{ lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super:
      {
        blockbench-electron =
          super.blockbench-electron.overrideAttrs (oldAttrs: rec {
          pname = "blockbench-electron";
          alias = "blockbench";
          version = "4.8.1";

          src = super.fetchurl {
            inherit pname version;
            url = "https://github.com/JannisX11/blockbench/releases/download/v${version}/Blockbench_${version}.AppImage";
            sha256 = "sha256-CE2wDOt1WBcYmPs4sEyZ3LYvKLequFZH0B3huMYHlwA=";
            name = "${pname}-${version}.AppImage";
            };

          appimageContents = super.appimageTools.extractType2 {
            inherit pname version;
            name = "${pname}-${version}";
            inherit src;
          };
          
          dontUnpack = true;
          dontConfigure = true;
          dontBuild = true;
          
          nativeBuildInputs = [ super.makeWrapper ];
          
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin $out/share/${pname} $out/share/applications
            cp -a ${appimageContents}/{locales,resources} $out/share/${pname}
            cp -a ${appimageContents}/blockbench.desktop $out/share/applications/${pname}.desktop
            cp -a ${appimageContents}/usr/share/icons $out/share
            substituteInPlace $out/share/applications/${pname}.desktop \
              --replace 'Exec=AppRun' 'Exec=${pname}'
            runHook postInstall
          '';
          
          postFixup = ''
            makeWrapper ${super.electron_22}/bin/electron $out/bin/${pname} \
              --add-flags $out/share/${pname}/resources/app.asar \
              --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [ super.stdenv.cc.cc ]}"

            makeWrapper ${super.electron_22}/bin/electron $out/bin/${alias} \
              --add-flags $out/share/${pname}/resources/app.asar \
              --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [ super.stdenv.cc.cc ]}"
          '';
          });
      }
    )
  ];

  home.packages = [ pkgs.blockbench-electron ];
}
