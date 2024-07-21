{ userSettings, ... }:

{
  imports = [ ../homelab/base.nix
              ( import ../../system/security/sshd.nix {
                authorizedKeys = [];
                inherit userSettings; })
            ];
}
