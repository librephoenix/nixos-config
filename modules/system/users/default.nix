{ config, lib, ... }:

{
  options = {
    systemSettings = {
      users = lib.mkOption {
        description = "List of desktop users to create on the system";
        type = lib.types.listOf lib.types.str;
      };
      adminUsers = lib.mkOption {
        description = "List of desktop users to grant admin (sudo) access on the system";
        type = lib.types.listOf lib.types.str;
      };
    };
  };
  config = {

    users.users = builtins.listToAttrs
      (map (user: {
        name = user;
        value = {
          isNormalUser = true;
          extraGroups = [ "networkmanager" "input" "dialout" "video" "render" ] ++ (lib.optionals (lib.any (x: x == user) config.systemSettings.adminUsers) [ "wheel" ]);
          createHome = true;
        };
      }) config.systemSettings.users);

    home-manager.users = builtins.listToAttrs
      (map (user: {
        name = user;
        value = {
          home.username = user;
          home.homeDirectory = "/home/"+user;
        };
      }) config.systemSettings.users);
  };
}
