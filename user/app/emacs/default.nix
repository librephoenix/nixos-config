{ config, pkgs, ... }:
{
  home.packages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacs-pgtk;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs: with epkgs; [
        command-log-mode
        vertico corfu hotfuzz orderless
        evil evil-collection evil-snipe evil-owl evil-vimish-fold
        dashboard doom-modeline
        nerd-icons nerd-icons-dired nerd-icons-corfu
        nerd-icons-ibuffer nerd-icons-completion
        yasnippet
        projectile treemacs
        magit magit-file-icons
        vterm
        gdscript-mode
        nix-mode
        python python-mode
      ];
    })
  ];
  home.file.".config/emacs/init.el".source = ./init.el;
}
