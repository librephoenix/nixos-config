{
  config,
  lib,
  pkgs,
  pkgs-stable,
  osConfig,
  ...
}:

let
  cfg = config.userSettings.git;
in
{
  options = {
    userSettings.git = {
      enable = lib.mkEnableOption "Enable git";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.git
      pkgs.git-extras
      pkgs.git-filter-repo
      pkgs-stable.openssh
    ];
    programs.git.enable = true;
    programs.git.userName = config.userSettings.name;
    programs.git.userEmail = config.userSettings.email;
    programs.git.aliases = {
      change-commits = ''!f() { VAR=$1; OLD=$2; NEW=$3; shift 3; FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch --env-filter "if [[ \"$`echo $VAR`\" = '$OLD' ]]; then export $VAR='$NEW'; fi" $@; }; f'';
    };
    programs.git.extraConfig = {
      init.defaultBranch = "main";
      safe.directory = [
        osConfig.systemSettings.dotfilesDir
        osConfig.systemSettings.secretsFlakeDir
        (config.home.homeDirectory + "/.cache/nix/tarball-cache")
      ];
    };
    programs.git.lfs.enable = true;
    services.ssh-agent.enable = true;
  };
}
