{ inputs, ... }:

let blocklist = builtins.readFile "${inputs.blocklist-hosts}/alternates/gambling-porn/hosts";
in
{
  networking.extraHosts = ''
    "${blocklist}"
  '';
}
