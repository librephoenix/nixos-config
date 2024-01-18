{ ... }:

{
  #services.auto-cpufreq.enable = true;
  #services.auto-cpufreq.settings = {
  #  charger = {
  #    governor = "performance";
  #    turbo = "auto";
  #  };
  #  battery = {
  #    governor = "schedutil";
  #    scaling_max_freq = 3800000;
  #    turbo = "never";
  #  };
  #};
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 80;
    };
  };

}
