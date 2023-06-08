{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ranger
    xdragon
  ];
  home.file.".config/ranger/rc.conf".source = ./rc.conf;
  home.file.".config/ranger/rifle.conf".source = ./rifle.conf;
  home.file.".config/ranger/scope.sh" = {
    source = ./scope.sh;
    executable = true;
  };
  home.file.".config/ranger/commands.py" = {
    source = ./commands.py;
    executable = true;
  };
  home.file.".config/ranger/commands_full.py" = {
    source = ./commands_full.py;
    executable = true;
  };
  home.file.".config/ranger/colorschemes/hail.py" = {
    source = ./colorschemes/hail.py;
    executable = true;
  };
}
