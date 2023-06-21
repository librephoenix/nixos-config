{ config, myName, pkgs, ... }:

{
  # Doas instead of sudo
  security.doas.enable = true;
  security.sudo.enable = false;
  security.doas.extraRules = [{
    users = [ "${myName}" ];
    keepEnv = true;
    persist = true;
  }];

  environment.systemPackages = [
    (pkgs.writeScriptBin "sudo" ''exec doas "$@"'')
  ];
}
