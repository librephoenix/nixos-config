{ config, pkgs, ... }:

{
  # Enable printing
  services.printing.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.openFirewall = true;
  environment.systemPackages = [ pkgs.cups-filters ];
}
