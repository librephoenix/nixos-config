{config, lib, ... }:

let
  cfg = config.userSettings.xdg;
in {
  options = {
    userSettings.xdg = {
      enable = lib.mkEnableOption "Enable xdg user dirs with my xdg directory structure";
    };
  };

  config = lib.mkIf cfg.enable {
    # TODO fix mime associations, most of them are totally broken :(
    xdg.enable = true;
    xdg.userDirs = {
      enable = true;
      createDirectories = true;
      music = "${config.home.homeDirectory}/Media/Music";
      videos = "${config.home.homeDirectory}/Media/Videos";
      pictures = "${config.home.homeDirectory}/Media/Pictures";
      templates = "${config.home.homeDirectory}/Templates";
      download = "${config.home.homeDirectory}/Downloads";
      documents = "${config.home.homeDirectory}/Documents";
      desktop = null;
      publicShare = null;
      extraConfig = {
        XDG_DOTFILES_DIR = "${config.home.homeDirectory}/.dotfiles";
        XDG_ARCHIVE_DIR = "${config.home.homeDirectory}/Archive";
        XDG_PROJECTS_DIR = "${config.home.homeDirectory}/Projects";
        XDG_CLOUD_DIR = "${config.home.homeDirectory}/Drive";
        XDG_BOOK_DIR = "${config.home.homeDirectory}/Media/Books";
        XDG_VM_DIR = "${config.home.homeDirectory}/Machines";
        XDG_NOTES_DIR = "${config.home.homeDirectory}/Notes";
        XDG_KP_DIR = "${config.home.homeDirectory}/KP";
        XDG_SCREENSHOT_DIR = "${config.home.homeDirectory}/Screenshots";
      };
    };
    xdg.mime.enable = true;
    xdg.mimeApps.enable = true;
  };
}
