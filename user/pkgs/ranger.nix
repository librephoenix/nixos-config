{ lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super:
      {
        ranger = super.ranger.overrideAttrs (oldAttrs: rec {
        preConfigure = ''
          substituteInPlace ranger/__init__.py \
            --replace "DEFAULT_PAGER = 'less'" "DEFAULT_PAGER = '${lib.getBin pkgs.bat}/bin/bat'"
      
          # give image previews out of the box when building with w3m
          substituteInPlace ranger/config/rc.conf \
            --replace "set preview_images false" "set preview_images true"

          # adds this patch: https://github.com/ranger/ranger/pull/1758
          # fixes a bug for kitty users that use image previews
          substituteInPlace ranger/ext/img_display.py \
            --replace "self.image_id -= 1" "self.image_id = max(0, self.image_id - 1)"

          # fixes the .desktop file
          substituteInPlace doc/ranger.desktop \
            --replace "Icon=utilities-terminal" "Icon=user-desktop"
          substituteInPlace doc/ranger.desktop \
            --replace "Terminal=true" "Terminal=false"
          substituteInPlace doc/ranger.desktop \
            --replace "Exec=ranger" "Exec=kitty -e ranger %U"
        '';
        });
      }
    )
  ];
  home.packages = with pkgs; [
    poppler_utils
    librsvg
    ffmpegthumbnailer
  ];

}
