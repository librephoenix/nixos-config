{ config, pkgs, ... }:

{
#  nixpkgs.overlays = [
#    (self: super:
#      {
#        keepmenu = super.keepmenu.overrideAttrs (oldAttrs: rec {
#        pname = "keepmenu";
#        version = "1.3.1";
#        src = super.python3Packages.fetchPypi {
#          inherit pname version;
#          sha256 = "sha256-AGuJY7IirzIjcu/nY9CzeOqU1liwcRijYLi8hGN/pRg=";
#        };
#        });
#      }
#    )
#  ];

  home.packages = with pkgs; [
    keepassxc
    keepmenu
  ];
}
