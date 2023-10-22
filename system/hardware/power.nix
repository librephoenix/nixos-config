{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ auto-cpufreq ];
  systemd.services.auto-cpufreq.enable = true;
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    charger = {
      governor = "performance";
      turbo = "auto";
    };
    battery = {
      governor = "schedutil";
      scaling_max_freq = 3800000;
      turbo = "never";
    };
  };

}
