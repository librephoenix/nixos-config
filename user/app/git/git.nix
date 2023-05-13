{ config, lib, pkgs, myName, myEmail, ... }:

{
  programs.git.enable = true;
  programs.git.userName = myName;
  programs.git.userEmail = myEmail;
  programs.git.extraConfig = {
    init.defaultBranch = "main";
  };
}
