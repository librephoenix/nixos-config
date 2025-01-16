{ lib, fetchurl, trivialBuild }:

trivialBuild {
  pname = "ultra-scroll";
  version = "0.2.0";

  src = fetchGit {
    url = "https://github.com/jdtsmith/ultra-scroll.git";
    rev = "64ad7be02e11317576498dabb15c92cf31e2c04c";
    ref = "main";
  };

  meta = with lib; {
    description = "scroll Emacs like lightning";
    homepage = "https://github.com/jdtsmith/ultra-scroll";
    license = licenses.gpl3;
    platforms = platforms.all;
  };
}
