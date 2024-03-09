{ userSettings, ... }:

{
  imports = [ ../homelab/base.nix
              ( import ../../system/security/sshd.nix {
                authorizedKeys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDBW6X3nd54sLK5UOvkyxBZ0dC57+xXKsAkr6XyP3k64gsFNahIYZGyZ+E9DdGIP1SZPDkwlcnGfFTPo/dPq5QjxtBmAOd/q4hIb5mqojNfOwqZQVwHjzLYSiGBaMVe4XAGCoNxTwXTimVU8wtyvlmBJzuOLmxZq9tRBpN3g+PT5HT1S9mrrQ4l5Y+2CNTwfga6/+/H1g4hpYG6H9qdIWOrel1hWvGUH3A1d/5mJIx3GkOAl7WBReQNbwlTm/8mkIcNBMtp3JQg5yuTQ6dLGwMOspMB5sGSCnaDx6CrelQglRihxkunW9ktA2MYaIn3l5fNwaHilW63d0sb9Y/Rr/bFKybmGYVY4PR9+tQg4D4YqofkSfRKmB98dtxUZg4yduXjxCYrhDTDseCjXACncItdD9TmxabaBQtWiWIGmX3bhx31SoctwdpS+mzbB5WVIVb/Fo/cOvPj6ugB5ZQf2uH+U5nsGVYsSsNGS1zVDybeILr1z2ne2AcaqwB5Z7iz/E0=" ];
                inherit userSettings; })
            ];
}
