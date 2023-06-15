#!/bin/sh
if [[ -f "$1" ]]; then
  TYPE=$(file -b --mime-type "$1")
  xclip -selection clipboard -t "$TYPE" -i "$1"
else
  echo $1 | xclip -selection clipboard -t text/plain &> /dev/null
  exit
fi
exit
