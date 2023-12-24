{ config, lib, pkgs, ... }:

{
  services.timesyncd.enable = true;
}
