{ config, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.auto-cpufreq ];
  systemd.services.auto-cpufreq.enable = true;

  services.upower.enable = true;
  systemd.services.upower.enable = true;
}
