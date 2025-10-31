{ ... }:

{
  config = {

    userSettings = {
      # setup
      shell = {
        enable = true;
        apps.enable = true;
      };
      xdg.enable = false;

      # programs
      yazi.enable = true;
      git.enable = true;
    };

  };
}
