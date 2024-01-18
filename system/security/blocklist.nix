{ blocklist-hosts, ... }:

let blocklist = builtins.readFile "${blocklist-hosts}/alternates/gambling-porn/hosts";
in
{
  networking.extraHosts = ''
    "${blocklist}"
  '';
}
