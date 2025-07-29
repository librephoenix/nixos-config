{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.emacs;
in {
  options = {
    userSettings.emacs = {
      enable = lib.mkEnableOption "Enable emacs";
      opacity = lib.mkOption {
        default = 85;
        type = lib.types.number;
        description = "Emacs' percentage opacity as a whole number";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      (pkgs.emacsWithPackagesFromUsePackage {
        config = ./init.el;
        package = pkgs.emacs-git-pgtk;
        alwaysEnsure = false;
        extraEmacsPackages = epkgs: with epkgs; [
          org-modern olivetti
          command-log-mode
          vertico corfu hotfuzz orderless
          evil evil-collection evil-snipe evil-owl evil-vimish-fold
          dashboard doom-themes doom-modeline
          nerd-icons nerd-icons-dired nerd-icons-corfu
          nerd-icons-ibuffer nerd-icons-completion
          yasnippet shackle
          projectile treemacs treemacs-projectile
          treemacs-evil treemacs-nerd-icons
          treesit-grammars.with-all-grammars
          git-timemachine wgrep
          magit magit-todos
          undo-fu undo-fu-session
          org-roam org-node org-node-fakeroam
          vterm vterm-toggle sudo-edit
          direnv
          gdscript-mode
          nix-mode
          python python-mode
          lsp-mode flycheck lsp-ui lsp-treemacs
          # fix ultra-scroll
          (epkgs.callPackage (
            { lib, fetchurl, trivialBuild }:
            
            trivialBuild {
              pname = "ultra-scroll";
              version = "0.2.0";
            
              src = builtins.fetchGit {
                url = "https://github.com/jdtsmith/ultra-scroll.git";
                rev = "64ad7be02e11317576498dabb15c92cf31e2c04c";
                ref = "main";
              };
            
              meta = with lib; {
                description = "scroll Emacs like lightning";
                homepage = "https://github.com/jdtsmith/ultra-scroll";
                license = licenses.gpl3;
                platforms = platforms.all;
              };
            }
          ) {})
          (epkgs.callPackage (
            { lib, fetchurl, trivialBuild }:
            
            trivialBuild {
              pname = "lsp-treemacs-nerd-icons";
              version = "2efa09a-unstable";
            
              src = builtins.fetchGit {
                url = "https://github.com/Velnbur/lsp-treemacs-nerd-icons.git";
                rev = "2efa09a701b8b455bfb66529454f27c30f7462dc";
                ref = "master";
              };

              buildInputs = with pkgs.emacsPackages; [
                lsp-treemacs
                treemacs
                nerd-icons
              ];
            
              meta = with lib; {
                description = "Use nerd icons for lsp-mode and lsp-treemacs";
                homepage = "https://github.com/Velnbur/lsp-treemacs-nerd-icons";
                license = licenses.gpl3;
                platforms = platforms.all;
              };
            }
          ) {})
          (epkgs.callPackage (
            { lib, fetchurl, trivialBuild }:
            
            trivialBuild {
              pname = "scratch-plus";
              version = "2efa09a-unstable";
            
              src = builtins.fetchGit {
                url = "https://git.sr.ht/~swflint/scratch-plus";
                rev = "b794901f968000f6e338808307385b683b79ec8b";
                ref = "main";
              };
            
              meta = with lib; {
                description = "Better scratch buffers";
                homepage = "https://git.sr.ht/~swflint/scratch-plus";
                license = licenses.gpl3;
                platforms = platforms.all;
              };
            }
          ) {})
        ];
      })
      fira-code
      nerd-fonts.fira-code
      nil
    ];
    home.file.".config/emacs/init.el".source = ./init.el;
    home.file.".config/emacs/lib".source = ./lib;
    home.file.".config/emacs/themes/doom-stylix-theme.el".source = config.lib.stylix.colors {
        template = builtins.readFile ./lib/doom-stylix-theme.el.mustache;
        extension = ".el";
    };
    home.file.".config/emacs/sysvars.el".text = ''
      ;;; sysvars.el --- imported variables from nixos config -*- lexical-binding: t; no-byte-compile: t; -*-
      ;;
      ;; Author: Emmet K <https://gitlab.com/librephoenix>
      ;; Maintainer: Emmet K <https://gitlab.com/librephoenix>
      ;;
      ;;; Commentary:
      ;;
      ;; Emacs variables imported from my NixOS config.
      ;;
      ;;; Code:
      (setq systemOpacity ${builtins.toString config.userSettings.emacs.opacity})

      ;;; sysvars.el ends here
    '';
    wayland.windowManager.hyprland.settings.exec-once = lib.optionals config.wayland.windowManager.hyprland.enable [ "emacs --daemon" ];
  };
}
