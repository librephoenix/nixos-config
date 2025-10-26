{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.blender;
in {
  options = {
    userSettings.blender = {
      enable = lib.mkEnableOption "Enable blender";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      [ pkgs.blender-hip
        (pkgs.writeScriptBin "declarative-blender-prefs"
          ''
          #!/bin/sh
          ${pkgs.blender-hip}/bin/blender --python ${./applyprefs.py};
          '')
      ];
    home.file.".config/declarative-blender-prefs/applyprefs.py".source = ./applyprefs.py;
    home.file.".config/blender/extensions/node_pie.zip".source = builtins.fetchurl {
      url = "https://github.com/strike-digital/node_pie/releases/download/1.2.38/node_pie_1_2_38.zip";
      sha256 = "sha256:00kscj7dkl80kc482jg3kcw9vhr1n64n44ld2xncr6gxil679fk2";
    };
    home.file.".config/blender/extensions/bool_tool.zip".source = builtins.fetchurl {
      name = "bool_tool";
      url = "https://extensions.blender.org/download/sha256:74ecd752ec3eda67153c74ea5a6b22709da2669a6da43264bfa291fc784306b3/add-on-bool-tool-v1.1.2.zip?repository=%2Fapi%2Fv1%2Fextensions%2F&blender_version_min=4.2.0";
      sha256 = "sha256:1cq68dwgr4d2pxj3593dk9ka57bh49mmmskl7hangniyxi9dgv3l";
    };
  };
}
