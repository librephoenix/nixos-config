{ config, lib, pkgs, ... }:
let
  myYtpScript = ''
    ytsub --video-player /usr/bin/mpv
  '';
  myYtaScript = ''
    yt-dlp -x --audio-format mp3 $1
  '';
  myYtdScript = ''
    pushd ~/Media/Podcasts;
    ytsub --video-player yta;
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
