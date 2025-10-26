{ ... }:

{
  config = {

    userSettings = {
      # setup
      shell = {
        enable = true;
        apps.enable = true;
        extraApps.enable = true;
      };
      xdg.enable = true;

      # programs
      browser = "brave";
      editor = "vscodium";
      yazi.enable = true;
      git.enable = true;
      art.enable = true;
      flatpak.enable = false;
      godot.enable = true;
      keepass.enable = true;
      media.enable = true;
      office.enable = true;

      # wm
      plasma.enable = true;

      # style
      stylix.enable = true;

      # hardware
      bluetooth.enable = true;
    };

  };
}
