# THIS DOES NOT WORK YET!
{ stdenv, pkgs, ... }:

let name = "rogauracore";
    owner = "wroberts";
    version = "1.6";
in
  stdenv.mkDerivation {
  inherit name owner version;
  src = fetchTarball {
    url = "https://github.com/"+owner+"/"+name+"/releases/download/"+version+"/rogauracore-"+version+".tar.gz";
    sha256 = "0vpypcq71yv0v8vbgpkn8xy77j22g55aw6i83s30mpbpjjna2lm9";
  };
  buildInputs = [ pkgs.udev
                  pkgs.libusb
                ];
}
