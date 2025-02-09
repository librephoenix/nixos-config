{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.brave;
in {
  options = {
    userSettings.brave = {
      enable = lib.mkEnableOption "Enable brave browser";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.brave ];

    nixpkgs.config.overlays = [
      (self: super: {
        brave = super.brave.override {
          commandLineArgs = [
            "--password-store=gnome-libsecret"
            "--ignore-gpu-blocklist"
            "--enable-gpu-rasterization"
            "--enable-accelerated-video-decode"
            "--enable-quic"
            "--enable-zero-copy"
            "--enable-native-gpu-memory-buffers"
            "--num-raster-threads=4"
          ];
        };
      })
    ];

    xdg.mimeApps.defaultApplications = lib.mkIf (config.userSettings.browser == "brave" ) {
      "text/html" = "brave-browser.desktop";
      "x-scheme-handler/http" = "brave-browser.desktop";
      "x-scheme-handler/https" = "brave-browser.desktop";
      "x-scheme-handler/about" = "brave-browser.desktop";
      "x-scheme-handler/unknown" = "brave-browser.desktop";
    };

    home.sessionVariables = lib.mkIf (config.userSettings.browser == "brave") {
      DEFAULT_BROWSER = "${pkgs.brave}/bin/brave";
    };
  };
}
