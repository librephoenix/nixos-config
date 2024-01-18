{ pkgs, ... }:
let
  myYtpScript = ''
    #!/bin/sh
    ytsub --video-player mpv
  '';
  myYtaScript = ''
    #!/bin/sh
    yt-dlp -x --audio-format mp3 $1
  '';
  myYtdScript = ''
    #!/bin/sh
    pushd ~/Media/Podcasts;
    ytsub --video-player ~/.nix-profile/bin/yta;
    popd;
  '';
in
{
  home.packages = [
    pkgs.yt-dlp
    (pkgs.writeScriptBin "ytp" myYtpScript)
    (pkgs.writeScriptBin "yta" myYtaScript)
    (pkgs.writeScriptBin "ytd" myYtdScript)
  ];
}
