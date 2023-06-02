{ config, lib, pkgs, eaf, eaf-browser, ... }:

{
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./.;
  };
  home.file.".emacs.d/themes/doom-stylix-theme.el".source = config.lib.stylix.colors {
      template = builtins.readFile ./themes/doom-stylix-theme.el.mustache;
      extension = ".el";
  };
  home.packages = with pkgs; [
  git
  nodejs
  wmctrl
  jshon
  aria
  hledger
  nodePackages.mermaid-cli
  (python3.withPackages (p: with p; [
    pandas
    requests
    pyqt6 sip qtpy qt6.qtwebengine epc lxml pyqt6-webengine
    pysocks
    pymupdf
    markdown
  ]))];
  home.file.".emacs.d/eaf" = {
    source = "${eaf}";
    recursive = true;
  };
  home.file.".emacs.d/eaf/app/browser" = {
    source = "${eaf-browser}";
    recursive = true;
    onChange = "
      pushd ~/.emacs.d/eaf/app/browser;
      rm package*.json;
      npm install darkreader @mozilla/readability && rm package*.json;
      popd;
    ";
  };
}
