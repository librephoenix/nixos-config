{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacs-pgtk;
      alwaysEnsure = true;
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
        magit magit-file-icons magit-todos
        undo-fu undo-fu-session
        org-roam org-node org-node-fakeroam
        vterm sudo-edit
        direnv
        gdscript-mode
        nix-mode
        python python-mode
        lsp-mode flycheck lsp-ui lsp-treemacs
        (epkgs.callPackage ./ultra-scroll.nix {})
      ];
    })
    fira-code
    nerd-fonts.fira-code
    nil
  ];
  home.file.".config/emacs/init.el".source = ./init.el;
  home.file.".config/emacs/themes/doom-stylix-theme.el".source = config.lib.stylix.colors {
      template = builtins.readFile ./doom-stylix-theme.el.mustache;
      extension = ".el";
  };
}
