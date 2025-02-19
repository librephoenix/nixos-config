{ config, lib, pkgs, ... }:

{
  config = {
    nixpkgs.config.overlays = [
      (self: super: {
        brave = super.brave.override {
          commandLineArgs = [
            "--password-store=gnome-libsecret"
            "--ignore-gpu-blocklist"
            "--enable-gpu-rasterization"
            "--enable-accelerated-video-decode"
            "--enable-quic"
            "--enable-zero-copy"
            "--enable-native-gpu-memory-buffers"
            "--num-raster-threads=4"
          ];
        };
      })
    ];
  };
}
