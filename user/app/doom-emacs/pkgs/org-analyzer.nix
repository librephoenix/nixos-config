{ stdenv, fetchFromGitHub, pkgs, ... }:
let name = "clj-org-analyzer";
    version = "1.0.2";
in stdenv.mkDerivation
{
  inherit name version;

  src = builtins.fetchurl {
    url = "https://github.com/rksm/clj-org-analyzer/releases/download/1.0.2/org-analyzer-1.0.2.jar";
    sha256 = "sha256:1j5c688yg6f5y6n86rf6vkwd1csn1y4dc716d5bczmyr2sgi9c67";
  };

  dontUnpack = true;

  installPhase = ''
    mkdir $out $out/bin;
    cp $src $out/bin/org-analyzer.jar;
    echo "#!/bin/sh
         ${pkgs.jdk}/bin/java -jar $out/bin/org-analyzer.jar $@" > $out/bin/org-analyzer
    chmod +x $out/bin/org-analyzer.jar
    chmod +x $out/bin/org-analyzer
  '';

}
