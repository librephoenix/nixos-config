{ userSettings, pkgs, ... }:

{
  # Doas instead of sudo
  security.doas.enable = true;
  security.sudo.enable = false;
  security.doas.extraRules = [
    {
      users = [ "${userSettings.username}" ];
      keepEnv = true;
      persist = true;
    }
    {
      users = [ "${userSettings.username}" ];
      cmd = "nix";
      noPass = true;
      keepEnv = true;
    }
    {
      users = [ "${userSettings.username}" ];
      cmd = "nixos-rebuild";
      noPass = true;
      keepEnv = true;
    }
    {
      users = [ "${userSettings.username}" ];
      cmd = "nix-collect-garbage";
      noPass = true;
      keepEnv = true;
    }
  ];

  environment.systemPackages = [
    pkgs.doas-sudo-shim
  ];
}
