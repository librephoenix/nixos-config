{ userSettings, ... }:

{
  imports = [ ../homelab/base.nix
              ( import ../../system/security/sshd.nix {
                # TODO add public ssh key for worklab
                authorizedKeys = [ ];
                inherit userSettings; })
            ];
}
