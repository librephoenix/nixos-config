{ config, lib, inputs, ... }:
let
  userInfo = import inputs.secrets.userInfo;
in {
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
          description = userInfo.${user}.name;
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
          userSettings.name = lib.mkIf (userInfo.${user} ? name) userInfo.${user}.name;
          userSettings.email = lib.mkIf (userInfo.${user} ? email ) userInfo.${user}.email;
        };
      }) config.systemSettings.users);
  };
}
