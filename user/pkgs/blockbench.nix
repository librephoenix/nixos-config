{ config, stdenv, fetchurl, appimageTools, makeWrapper, electron_22, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super:
      {
        blockbench-electron =
        let
        inherit (pkgs) fetchurl stdenv appimageTools makeWrapper electron_22;
        in
          super.blockbench-electron.overrideAttrs (oldAttrs: rec {
          pname = "blockbench-electron";
          version = "4.8.1";
          src = fetchurl {
            inherit pname version;
            url = "https://github.com/JannisX11/blockbench/releases/download/v${version}/Blockbench_${version}.AppImage";
            sha256 = "sha256-CE2wDOt1WBcYmPs4sEyZ3LYvKLequFZH0B3huMYHlwA=";
            name = "${pname}-${version}.AppImage";
            };

          appimageContents = appimageTools.extractType2 {
            inherit pname version;
            name = "${pname}-${version}";
            inherit src;
          };
          
          dontUnpack = true;
          dontConfigure = true;
          dontBuild = true;
          
          nativeBuildInputs = [ makeWrapper ];
          
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
            makeWrapper ${electron_22}/bin/electron $out/bin/${pname} \
              --add-flags $out/share/${pname}/resources/app.asar \
              --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [ stdenv.cc.cc ]}"
          '';
          });
      }
    )
  ];
}
