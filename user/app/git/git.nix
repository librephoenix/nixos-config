{ config, lib, pkgs, name, email, ... }:

{
  home.packages = [ pkgs.git ];
  programs.git.enable = true;
  programs.git.userName = name;
  programs.git.userEmail = email;
  programs.git.extraConfig = {
    init.defaultBranch = "main";
  };
}
