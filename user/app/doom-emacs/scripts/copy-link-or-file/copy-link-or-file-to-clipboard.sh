#!/bin/sh
#command -v xclip >/dev/null 2>&1 || { echo "Need command xclip. Aborting." >&2; exit 1; }
if [[ -f "$1" ]]; then
  TYPE=$(file -b --mime-type "$1")
  xclip -selection clipboard -t "$TYPE" -i "$1"
else
  echo $1 | xclip -selection clipboard -t text/plain &> /dev/null
  exit
fi
exit
