{ pkgs, ... }:

{
  services.libinput = {
    enable = true;
    mouse.naturalScrolling = true;
    touchpad.naturalScrolling = true;
  };
  environment.systemPackages = [ pkgs.libinput ];
}
