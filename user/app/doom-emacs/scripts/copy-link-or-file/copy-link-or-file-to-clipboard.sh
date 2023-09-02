#!/bin/sh
if [ $(echo $XDG_SESSION_TYPE) == "wayland" ]; then
  FILENAME="$(wl-paste)"
  FILTEREDFILENAME=$(echo "$FILENAME" | sed "s+file:+./+")
  echo "$FILTEREDFILENAME"
  if [[ -f "$FILTEREDFILENAME" ]]; then
    wl-copy < "$FILTEREDFILENAME"
  fi
elif [ $(echo $XDG_SESSION_TYPE) == "x11" ]; then
  FILENAME="$(xclip -o)"
  FILTEREDFILENAME=$(echo "$FILENAME" | sed "s+file:+./+")
  if [[ -f "$FILTEREDFILENAME" ]]; then
    TYPE=$(file -b --mime-type "$FILTEREDFILENAME")
    xclip -selection clipboard -t "$TYPE" -i "$FILTEREDFILENAME"
    exit
  fi
else
  exit
fi
exit
