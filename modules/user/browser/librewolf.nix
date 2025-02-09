{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.librewolf;
in {
  options = {
    userSettings.librewolf = {
      enable = lib.mkEnableOption "Enable librewolf browser";
    };
  };

  config = lib.mkIf cfg.enable {
    # Module installing librewolf as default browser
    home.packages = [ pkgs.librewolf ];

    home.file.".librewolf/librewolf.overrides.cfg".text = ''
      defaultPref("font.size.variable.x-western",20);
      defaultPref("browser.toolbars.bookmarks.visibility","always");
      defaultPref("privacy.resisttFingerprinting.letterboxing", true);
      defaultPref("network.http.referer.XOriginPolicy",2);
      defaultPref("privacy.clearOnShutdown.history",true);
      defaultPref("privacy.clearOnShutdown.downloads",true);
      defaultPref("privacy.clearOnShutdown.cookies",true);
      defaultPref("gfx.webrender.software.opengl",false);
      defaultPref("webgl.disabled",true);

      pref("font.size.variable.x-western",20);
      pref("browser.toolbars.bookmarks.visibility","always");
      pref("privacy.resisttFingerprinting.letterboxing", true);
      pref("network.http.referer.XOriginPolicy",2);
      pref("privacy.clearOnShutdown.history",true);
      pref("privacy.clearOnShutdown.downloads",true);
      pref("privacy.clearOnShutdown.cookies",true);
      pref("gfx.webrender.software.opengl",false);
      pref("webgl.disabled",true);
      '';

    xdg.mimeApps.defaultApplications = lib.mkIf (config.userSettings.browser == "librewolf") {
      "text/html" = "librewolf.desktop";
      "x-scheme-handler/http" = "librewolf.desktop";
      "x-scheme-handler/https" = "librewolf.desktop";
      "x-scheme-handler/about" = "librewolf.desktop";
      "x-scheme-handler/unknown" = "librewolf.desktop";
    };

    home.sessionVariables = lib.mkIf (config.userSettings.browser == "librewolf") {
      DEFAULT_BROWSER = "${pkgs.librewolf}/bin/librewolf";
    };
  };

}
