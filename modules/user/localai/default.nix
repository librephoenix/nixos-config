{ config, lib, pkgs, pkgs-stable, ...}:

let
  cfg = config.userSettings.ai;
in {
  options = {
    userSettings.ai = {
      enable = lib.mkEnableOption "Enable localai";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs-stable.local-ai
                      (pkgs.writeScriptBin "aid"
                        ''
                        # ai daemon
                        pushd ~/.config/local-ai;
                        local-ai &> /dev/null & disown;
                        popd;'')
                    ];
  };
}
