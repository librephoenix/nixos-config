{ lib, fetchgit, pkgs, ... }:
let
  rustPlatform = pkgs.makeRustPlatform {
    cargo = pkgs.rust-bin.stable.latest.minimal;
    rustc = pkgs.rust-bin.stable.latest.minimal;
  };
in
rustPlatform.buildRustPackage rec {
  pname = "russ";
  version = "unstable";

  src = fetchgit {
    url = "https://github.com/ckampfe/russ.git";
    rev = "1482bb1df13738fdd4ea1badf2146a9ed8e6656e";
    sha256 = "sha256-MvTMo2q/cQ/LQNdUV8SmHgGlA42kLl0i9mdcoAFV/I4=";
  };

  cargoSha256 = "sha256-/r1Dp7eh2qVYRGINVdPq6e8c9U/R2AzVEy/g+j/GRPo=";

  checkType = "debug";
  checkFlags = [
    # network required tests don't work when building with nix for some reason
    "--skip=rss::tests::it_fetches"
    "--skip=rss::tests::it_subscribes_to_a_feed"
    "--skip=rss::tests::refresh_feed_does_not_add_any_items_if_there_are_no_new_items"
  ];

  meta = with lib; {
    description = "A TUI RSS reader with vim-like controls and a local-first, offline-first focus ";
    homepage = "https://github.com/ckampfe/russ";
    license = licenses.agpl3Only;
    maintainers = [];
  };
}
