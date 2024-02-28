{ userSettings, ... }:

{
  imports = [ ./base.nix
              ( import ../../system/security/sshd.nix {
                authorizedKeys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDaeejVJwUVrIZSo1isbu+gkQ7+8ftCgCsczy3OclkEVWHyRTqlG6yp74hr3j8ZNsOhov7c2Q6RqC8oy669hlxi/y9BsvtlI7sBr94oAKFOmkCS4RiK72ngJjBvI0vbk89wQQjmAd3r8B7ZcedpNOC8CkHu8SebKdYPRIUvAbPc3fTEt7DsJkazAepZCB8LEhUp57FAqQ/Ezlt3X/1uwNq5S0EbE9Zm+nUpEfSqR9apY2neKWLyGiCxpK3dzyNOuulCxvtVz+ie2sTk/6SxM+qWEoVVxhdwyxPihEjgC0EvtG0S5mVh5JmcjRkJOzzBHJuw+6r8yWn/AxGdIsoJ4rKNxH1XH1iLHgCraOLOUjUNlmejTcQPu6o92a79fdz2gCHT/BuIjfCW7MErAC3YSmF45TSur/kiWCBaTqYo06pgbQ3w1vKg7fievQlQzsutmg47RvJp6fb74yxuOdVg39cShQu/l8r6zqm21JAeUaaIp4P/0MrAIMOOVUhbK0QgsNElO4yn0ZKH8wGIF8xORh7ikxUIAyq8C41gjJiO2sAFJc3M8DhduQU3X0lHB7U0Qyu+8ZXn05+zdFPXJ73LKc7DCcLkppRXJsdHLSDEFdWqFnV7o08B4qZkPMT4pmvhwhY0Pf1fwavOqxuTstzw18gUGyQzl0foQi0Qrmdazsp2Qw== emmet@snowfire"];
                inherit userSettings; })
            ];
}
