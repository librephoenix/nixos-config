{ pkgs, inputs, ... }:

{
  home.packages = with pkgs; [
    neovim
    neovide
  ];
  home.file.".config/nvim".source = inputs.nvchad;
  home.file.".config/nvim".recursive = true;
}
