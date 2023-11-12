{ config, pkgs, ... }:

{
  services.journald.extraConfig = "SystemMaxUse=250M\nSystemMaxFiles=10";
  services.journald.rateLimitBurst = 800;
  services.journald.rateLimitInterval = "5s";
}
