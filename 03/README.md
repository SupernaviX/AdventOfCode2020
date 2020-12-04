```
JAVA_PATH="$JAVA_HOME/bin/java"
SAXON_PATH=/path/to/saxon-he-10.3.jar

"$JAVA_PATH" -jar "$SAXON_PATH" -s:input.xml -xsl:part1.xslt -o:output1.xml
"$JAVA_PATH" -jar "$SAXON_PATH" -s:input.xml -xsl:part2.xslt -o:output2.xml
```