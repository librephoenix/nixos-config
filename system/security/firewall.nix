{ ... }:

{
  # Firewall
  networking.firewall.enable = true;
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22000 21027 ]; # syncthing
  networking.firewall.allowedUDPPorts = [ 22000 21027 ]; # syncthing
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
}
