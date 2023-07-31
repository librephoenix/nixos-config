{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.prismlauncher ];
}
