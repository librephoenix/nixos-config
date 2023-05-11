#!/bin/sh

filename=$1
echo $filename
stylefile=$2
echo $stylefile

filenamebase=$(basename "$filename")
filenameext="${filenamebase##*.}"
echo $filenameext

if [ $filenameext = "org" ]; then
   stylefilebase=$(basename "$stylefile")
   stylefileext="${stylefilebase##*.}"

   if [ $stylefileext = "odp" ]; then
       output="${filename//\.org/\.pptx}"
       finaloutput="${filename//\.org/\.odp}"
       pandoc "$filename" -o "$output"
       soffice --convert-to odp "$output"
       unzip "$finaloutput" content.xml
       unzip "$stylefile" styles.xml

       sed 's~</text:span>~~g' content.xml
       sed 's~<text:span text:style-name="..">~~g' content.xml

       python3 ~/.doom.d/scripts/ox-odp/ox-odp-xml-parse.py

       zip -d $finaloutput styles.xml
       zip -m $finaloutput styles.xml

       zip -d $finaloutput content.xml
       zip -m $finaloutput content.xml

       rm $output

       exit

   else
       echo "Style file is not an odp file."
   fi
else
    echo "Base file is not an org file."
    exit
fi

exit
