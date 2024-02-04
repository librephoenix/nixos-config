{ lib, pkgs, ... }:
let
  rustPlatform = pkgs.makeRustPlatform {
    cargo = pkgs.rust-bin.stable.latest.minimal;
    rustc = pkgs.rust-bin.stable.latest.minimal;
  };
in
rustPlatform.buildRustPackage rec {
  pname = "smartcalc-tui";
  version = "1.0.7";

  src = fetchTarball {
    url = "https://github.com/superhawk610/smartcalc-tui/archive/refs/tags/v1.0.7.tar.gz";
    sha256 = "1dv24rsj87avpbrmab0hy3v729fdqh1cfbvl1xsjmfn8y35z4m5m";
  };

  cargoSha256 = "sha256-0AWsJccfzkUkpB6imibN6iUNDEx3vrX9kEgD98nXURw=";

  checkType = "debug";

  meta = with lib; {
    description = "Terminal UI for erhanbaris/smartcalc";
    homepage = "https://github.com/superhawk610/smartcalc-tui";
    license = licenses.mit;
    maintainers = [];
  };
}
