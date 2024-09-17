{ config, lib, pkgs, pkgs-emacs, pkgs-stable, inputs, userSettings, systemSettings, ... }:
let
  themePolarity = lib.removeSuffix "\n" (builtins.readFile (./. + "../../../../themes"+("/"+userSettings.theme)+"/polarity.txt"));
  dashboardLogo = ./. + "/nix-" + themePolarity + ".webp";
in
{
  imports = [
    inputs.nix-doom-emacs.hmModule
    ../git/git.nix
    ../../shell/sh.nix
    ../../shell/cli-collection.nix
  ];

  programs.doom-emacs = {
    enable = true;
    emacsPackage = pkgs-emacs.emacs29-pgtk;
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
      in pkgs-emacs.linkFarm "doom-packages-dir" [
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
          path = pkgs-emacs.emptyFile;
        }
      ];
  # End block
  };

  home.file.".emacs.d/themes/doom-stylix-theme.el".source = config.lib.stylix.colors {
      template = builtins.readFile ./themes/doom-stylix-theme.el.mustache;
      extension = ".el";
  };

  home.packages = (with pkgs-emacs; [
    emacs-lsp-booster
    file
    wmctrl
    jshon
    aria
    hledger
    hunspell hunspellDicts.en_US-large
    (pkgs-emacs.mu.override { emacs = emacs29-pgtk; })
    (pkgs.callPackage ./pkgs/org-analyzer.nix {})
    emacsPackages.mu4e
    isync
    msmtp
    (python3.withPackages (p: with p; [
      pandas
      requests
      epc lxml
      pysocks
      pymupdf
      markdown
    ]))
  ]) ++ (with pkgs-stable; [
    nodejs
    nodePackages.mermaid-cli
  ]) ++ (with pkgs; [
    openssl
    stunnel
  ]);

  services.mbsync = {
    enable = true;
    package = pkgs-stable.isync;
    frequency = "*:0/5";
  };

  home.file.".emacs.d/org-yaap" = {
    source = "${inputs.org-yaap}";
    recursive = true;
  };

  home.file.".emacs.d/org-side-tree" = {
    source = "${inputs.org-side-tree}";
    recursive = true;
  };

  home.file.".emacs.d/org-timeblock" = {
    source = "${inputs.org-timeblock}";
    recursive = true;
  };

  home.file.".emacs.d/org-nursery" = {
    source = "${inputs.org-nursery}";
  };

  home.file.".emacs.d/org-krita" = {
    source = "${inputs.org-krita}";
  };

  home.file.".emacs.d/org-xournalpp" = {
    source = "${inputs.org-xournalpp}";
  };

  home.file.".emacs.d/org-sliced-images" = {
    source = "${inputs.org-sliced-images}";
  };

  home.file.".emacs.d/magit-file-icons" = {
    source = "${inputs.magit-file-icons}";
  };

  home.file.".emacs.d/dashboard-logo.webp".source = dashboardLogo;
  home.file.".emacs.d/scripts/copy-link-or-file/copy-link-or-file-to-clipboard.sh" = {
    source = ./scripts/copy-link-or-file/copy-link-or-file-to-clipboard.sh;
    executable = true;
  };

  home.file.".emacs.d/phscroll" = {
    source = "${inputs.phscroll}";
  };

  home.file.".emacs.d/mini-frame" = {
    source = "${inputs.mini-frame}";
  };

  home.file.".emacs.d/system-vars.el".text = ''
  ;;; ~/.emacs.d/config.el -*- lexical-binding: t; -*-

  ;; Import relevant variables from flake into emacs

  (setq user-full-name "''+userSettings.name+''") ; name
  (setq user-username "''+userSettings.username+''") ; username
  (setq user-mail-address "''+userSettings.email+''") ; email
  (setq user-home-directory "/home/''+userSettings.username+''") ; absolute path to home directory as string
  (setq user-default-roam-dir "''+userSettings.defaultRoamDir+''") ; absolute path to home directory as string
  (setq system-nix-profile "''+systemSettings.profile+''") ; what profile am I using?
  (setq system-wm-type "''+userSettings.wmType+''") ; wayland or x11?
  (setq doom-font (font-spec :family "''+userSettings.font+''" :size 20)) ; import font
  (setq dotfiles-dir "''+userSettings.dotfilesDir+''") ; import location of dotfiles directory
 '';
}
