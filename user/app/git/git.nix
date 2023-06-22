{ config, lib, pkgs, username, email, ... }:

{
  home.packages = [ pkgs.git ];
  programs.git.enable = true;
  programs.git.userName = username;
  programs.git.userEmail = email;
  programs.git.extraConfig = {
    init.defaultBranch = "main";
  };
}
