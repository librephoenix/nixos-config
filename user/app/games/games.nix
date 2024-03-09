{ pkgs, ... }:
let
  myRetroarch =
    (pkgs.retroarch.override {
      cores = with pkgs.libretro; [
        vba-m
        (desmume.overrideAttrs (oldAttrs: {
          preConfigure = ''
            sed -i 's/0009BF123456/0022AA067857/g' desmume/src/firmware.cpp;
            sed -i 's/outConfig.MACAddress\[0\] = 0x00/outConfig.MACAddress[0] = 0x00/g' desmume/src/firmware.cpp;
            sed -i 's/outConfig.MACAddress\[1\] = 0x09/outConfig.MACAddress[1] = 0x22/g' desmume/src/firmware.cpp;
            sed -i 's/outConfig.MACAddress\[2\] = 0xBF/outConfig.MACAddress[2] = 0xAA/g' desmume/src/firmware.cpp;
            sed -i 's/outConfig.MACAddress\[3\] = 0x12/outConfig.MACAddress[3] = 0x06/g' desmume/src/firmware.cpp;
            sed -i 's/outConfig.MACAddress\[4\] = 0x34/outConfig.MACAddress[4] = 0x78/g' desmume/src/firmware.cpp;
            sed -i 's/outConfig.MACAddress\[5\] = 0x56/outConfig.MACAddress[5] = 0x57/g' desmume/src/firmware.cpp;
            sed -i 's/0x00, 0x09, 0xBF, 0x12, 0x34, 0x56/0x00, 0x22, 0xAA, 0x06, 0x78, 0x57/g' desmume/src/wifi.cpp;
          '';
        }))
        dolphin
        genesis-plus-gx
      ];
    });
in
{
  home.packages = with pkgs; [
    # Games
    pegasus-frontend
    myRetroarch
    libfaketime
    airshipper
    qjoypad
    superTux
    superTuxKart

    # I installed these in distrobox
    # and exported using distrobox-export
    (pkgs.makeDesktopItem {
      name = "pokefinder";
      desktopName = "PokeFinder";
      exec = "/home/emmet/.local/bin/pokefinder";
      terminal = false;
      type = "Application";
    })
    (pkgs.makeDesktopItem {
      name = "eontimer";
      desktopName = "EonTimer";
      exec = "/home/emmet/.local/bin/eontimer";
      terminal = false;
      type = "Application";
    })
  ];

  nixpkgs.config = {
    allowUnfree = true;
    allowUnfreePredicate = (_: true);
  };

  # The following 2 declarations allow retroarch to be imported into gamehub
  # Set retroarch core directory to ~/.local/bin/libretro
  # and retroarch core info directory to ~/.local/share/libretro/info
  home.file.".local/bin/libretro".source = "${myRetroarch}/lib/retroarch/cores";
  home.file.".local/bin/libretro-shaders".source = "${myRetroarch}/lib/retroarch/cores";
  home.file.".local/share/libretro/info".source = fetchTarball {
    url = "https://github.com/libretro/libretro-core-info/archive/refs/tags/v1.15.0.tar.gz";
    sha256 = "004kgbsgbk7hn1v01jg3vj4b6dfb2cp3kcp5hgjyl030wqg1r22q";
  };

}
