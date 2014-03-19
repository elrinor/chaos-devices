#!/bin/bash
mkdir palette
cd palette
grep -E "products_pictures/dmc-" ../palette.htm | sed -r -e "s/^.*(products.*gif).*$/http\:\/\/www\.stezhok\.com\/\1/gi" | xargs wget
cd ..

rm palette.xml
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" >> palette.xml
echo "<flossScheme>" >> palette.xml
echo "<title>DMC</title>" >> palette.xml
for i in palette/*.gif
do
  desc=`basename $i .gif`
	name=`echo "$desc" | sed -r -e "s/.*-([0-9]+)/\1/gi"`
  ../../bin/release/cpicker -i "$i" -f "<floss><name>$name</name><description>$desc</description><color><red>\$red</red><green>\$green</green><blue>\$blue</blue></color></floss>" >>palette.xml
	echo "" >> palette.xml
done
echo "</flossScheme>\n" >> palette.xml
