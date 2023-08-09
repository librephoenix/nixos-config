{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ auto-cpufreq thermald ];
  systemd.services.auto-cpufreq.enable = true;
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    charger = {
      governor = "performance";
      turbo = "auto";
    };
    battery = {
      governor = "powersave";
      scaling_min_freq = 1200000;
      scaling_max_freq = 2200000;
      turbo = "never";
    };
  };

  services.thermald.enable = true;

  services.upower.enable = true;
  systemd.services.upower.enable = true;
}
