{ config, lib, pkgs, eaf, eaf-browser, org-nursery, theme, ... }:
let
  themePolarity = lib.removeSuffix "\n" (builtins.readFile (./. + "../../../../themes"+("/"+theme)+"/polarity.txt"));
  dashboardLogo = ./. + "/nix-" + themePolarity + ".png";
in
{
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./.;
    # This block from https://github.com/znewman01/dotfiles/blob/be9f3a24c517a4ff345f213bf1cf7633713c9278/emacs/default.nix#L12-L34
    # Only init/packages so we only rebuild when those change.
    doomPackageDir = let
      filteredPath = builtins.path {
        path = ./.;
        name = "doom-private-dir-filtered";
        filter = path: type:
          builtins.elem (baseNameOf path) [ "init.el" "packages.el" ];
      };
      in pkgs.linkFarm "doom-packages-dir" [
        {
          name = "init.el";
          path = "${filteredPath}/init.el";
        }
        {
          name = "packages.el";
          path = "${filteredPath}/packages.el";
        }
        {
          name = "config.el";
          path = pkgs.emptyFile;
        }
      ];
  # End block
  };

  home.file.".emacs.d/themes/doom-stylix-theme.el".source = config.lib.stylix.colors {
      template = builtins.readFile ./themes/doom-stylix-theme.el.mustache;
      extension = ".el";
  };

  home.packages = with pkgs; [
  git
  file
  nodejs
  wmctrl
  jshon
  aria
  hledger
  hunspell hunspellDicts.en_US-large
  pandoc
  nodePackages.mermaid-cli
  (python3.withPackages (p: with p; [
    pandas
    requests
    pyqt6 sip qtpy qt6.qtwebengine epc lxml pyqt6-webengine
    pysocks
    pymupdf
    markdown
  ]))];

  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

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
  home.file.".emacs.d/org-nursery" = {
    source = "${org-nursery}";
  };
  home.file.".emacs.d/dashboard-logo.png".source = dashboardLogo;
  home.file.".emacs.d/scripts/copy-link-or-file/copy-link-or-file-to-clipboard.sh" = {
    source = ./scripts/copy-link-or-file/copy-link-or-file-to-clipboard.sh;
    executable = true;
  };
}
