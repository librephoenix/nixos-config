{ lib, ... }:

{
  options = {
    userSettings = {
      name = lib.mkOption {
        default = "";
        description = "User full name";
        type = lib.types.str;
      };
      email = lib.mkOption {
        default = "";
        description = "User email";
        type = lib.types.str;
      };
    };
  };
}
