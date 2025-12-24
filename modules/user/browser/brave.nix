{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.userSettings.brave;
in
{
  options = {
    userSettings.brave = {
      enable = lib.mkEnableOption "Enable brave browser";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.brave.enable = true;
    programs.brave.package = pkgs.brave;
    programs.brave.extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock
      { id = "oboonakemofpalcgghocfoadofidjkkk"; } # keepassxc
      { id = "kgcjekpmcjjogibpjebkhaanilehneje"; } # karakeep
      { id = "abbpaepbpakcpipajigmlpnhlnbennna"; } # yt music mode
      { id = "gebbhagfogifgggkldgodflihgfeippi"; } # youtube dislikes
    ];
    programs.brave.commandLineArgs = [
      "--password-store=gnome-libsecret"
      "--enable-accelerated-video-decode"
      "--disable-features=WaylandWpColorManagerV1"
    ];
    xdg.mimeApps.defaultApplications = lib.mkIf (config.userSettings.browser == "brave") {
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
