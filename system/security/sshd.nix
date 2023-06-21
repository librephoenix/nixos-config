{ config, pkgs, ... }:

{
  # Enable incoming ssh
  services.openssh = {
    enable = true;
    openFirewall = true;
    # TODO authorizedKeysFiles = "";
  };
}
