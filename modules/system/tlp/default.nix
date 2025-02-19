{ config, lib, ... }:

let
  cfg = config.systemSettings.tlp;
in {
  options = {
    systemSettings.tlp = {
      enable = lib.mkEnableOption "Enable tlp power management";
    };
  };

  config = lib.mkIf cfg.enable {
    services.tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "interactive";
        CPU_SCALING_GOVERNOR_ON_BAT = "ondemand";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance";
        CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
        CPU_DRIVER_OPMODE_ON_AC = "active";
        CPU_DRIVER_OPMODE_ON_BAT = "active";

        WIFI_PWR_ON_AC = "on";
        WIFI_PWR_ON_BAT = "on";
        RUNTIME_PM_ON_AC = "auto";
        RUNTIME_PM_ON_BAT = "auto";

        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 100;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 70;

        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;
        CPU_HWP_DYN_BOOST_ON_AC = 1;
        CPU_HWP_DYN_BOOST_ON_BAT = 0;

        START_CHARGE_THRESH_BAT0 = 75;
        STOP_CHARGE_THRESH_BAT0 = 80;

        MEM_SLEEP_ON_AC = "deep";
        MEM_SLEEP_ON_BAT = "deep";

        PLATFORM_PROFILE_ON_AC = "quiet";
        PLATFORM_PROFILE_ON_BAT = "quiet";

        RADEON_DPM_STATE_ON_AC = "performance";
        RADEON_DPM_STATE_ON_BAT = "battery";
        RADEON_POWER_PROFILE_ON_AC = "high";
        RADEON_POWER_PROFILE_ON_BAT = "low";

        INTEL_GPU_MIN_FREQ_ON_AC = 600;
        INTEL_GPU_MIN_FREQ_ON_BAT = 600;
      };
    };
  };

}
