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
      ranger.enable = true;
      yazi.enable = true;
      git.enable = true;
    };

  };
}
