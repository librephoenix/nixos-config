{ lib, pkgs, ... }:
let
  rustPlatform = pkgs.makeRustPlatform {
    cargo = pkgs.rust-bin.stable.latest.minimal;
    rustc = pkgs.rust-bin.stable.latest.minimal;
  };
in
rustPlatform.buildRustPackage rec {
  pname = "ytsub";
  version = "0.4.0";

  src = fetchTarball {
    url = "https://github.com/sarowish/ytsub/archive/refs/tags/v0.4.0.tar.gz";
    sha256 = "1124mf5r2507d2939833xkavy2vi2rbws67dkim4vwah376k3rlf";
  };

  cargoSha256 = "sha256-pv4eKD2XgaDAJqSf3JzfnsayofmOSy4XRzZ8rkZrHAo=";

  buildNoDefaultFeatures = true;
  buildFeatures = [ "bundled_sqlite" ];

  meta = with lib; {
    description = "A subscriptions only TUI Youtube client that uses the Invidious API";
    homepage = "https://github.com/sarowish/ytsub";
    license = licenses.gpl3Only;
    maintainers = [];
  };
}
