{ config, lib, pkgs, ... }:

{
  # Module installing librewolf as default browser
  home.packages = [ pkgs.librewolf ];

  home.file.".librewolf/librewolf.overrides.cfg".text = ''
    defaultPref("font.name.serif.x-western","Inconsolata");
    defaultPref("font.size.variable.x-western",20);
    defaultPref("browser.toolbars.bookmarks.visibility","always");
    defaultPref("privacy.resisttFingerprinting.letterboxing", true);
    defaultPref("network.http.referer.XOriginPolicy",2);
    defaultPref("privacy.clearOnShutdown.history",false);
    defaultPref("privacy.clearOnShutdown.downloads",false);
    defaultPref("privacy.clearOnShutdown.cookies",false);
    defaultPref("gfx.webrender.software.opengl",true);
    defaultPref("webgl.disabled",false);
    pref("font.name.serif.x-western","Inconsolata");
    pref("font.size.variable.x-western",20);
    pref("browser.toolbars.bookmarks.visibility","always");
    pref("privacy.resisttFingerprinting.letterboxing", true);
    pref("network.http.referer.XOriginPolicy",2);
    pref("privacy.clearOnShutdown.history",false);
    pref("privacy.clearOnShutdown.downloads",false);
    pref("privacy.clearOnShutdown.cookies",false);
    pref("gfx.webrender.software.opengl",true);
    pref("webgl.disabled",false);
    '';

  xdg.mimeApps.defaultApplications = {
  "text/html" = "librewolf.desktop";
  "x-scheme-handler/http" = "librewolf.desktop";
  "x-scheme-handler/https" = "librewolf.desktop";
  "x-scheme-handler/about" = "librewolf.desktop";
  "x-scheme-handler/unknown" = "librewolf.desktop";
  };

  home.sessionVariables = {
    DEFAULT_BROWSER = "${pkgs.librewolf}/bin/librewolf";
  };

}
