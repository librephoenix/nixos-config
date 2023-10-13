{ config, lib, pkgs, eaf, eaf-browser, org-nursery, phscroll, org-yaap, org-side-tree, org-timeblock, theme, font, name, username, email, dotfilesDir, profile, wmType, defaultRoamDir, ... }:
let
  themePolarity = lib.removeSuffix "\n" (builtins.readFile (./. + "../../../../themes"+("/"+theme)+"/polarity.txt"));
  dashboardLogo = ./. + "/nix-" + themePolarity + ".png";
in
{
  programs.doom-emacs = {
    enable = true;
    emacsPackage = pkgs.emacs29-pgtk;
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
  nil
  nixfmt
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

  home.file.".emacs.d/org-yaap" = {
    source = "${org-yaap}";
    recursive = true;
  };

  home.file.".emacs.d/org-side-tree" = {
    source = "${org-side-tree}";
    recursive = true;
  };

  home.file.".emacs.d/org-timeblock" = {
    source = "${org-timeblock}";
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

  home.file.".emacs.d/phscroll" = {
    source = "${phscroll}";
  };

  home.file.".emacs.d/system-vars.el".text = ''
  ;;; ~/.emacs.d/config.el -*- lexical-binding: t; -*-

  ;; Import relevant variables from flake into emacs

  (setq user-full-name "''+name+''") ; name
  (setq user-username "''+username+''") ; username
  (setq user-mail-address "''+email+''") ; email
  (setq user-home-directory "/home/''+username+''") ; absolute path to home directory as string
  (setq user-default-roam-dir "''+defaultRoamDir+''") ; absolute path to home directory as string
  (setq system-nix-profile "''+profile+''") ; what profile am I using?
  (setq system-wm-type "''+wmType+''") ; wayland or x11?
  (setq doom-font (font-spec :family "''+font+''" :size 20)) ; import font
  (setq dotfiles-dir "''+dotfilesDir+''") ; import location of dotfiles directory
 '';
}
