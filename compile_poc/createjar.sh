#generates jars with compiler plugins which can be then directly used with scalac from commandline


rm -rf classes
mkdir classes
fsc -d classes src/main/scala/compile/FactorialParser.scala 
cp scalac-plugin-parser.xml classes/scalac-plugin.xml
(cd classes; jar cf ../factparser.jar .)

rm -rf classes
mkdir classes
fsc -d classes src/main/scala/compile/FactorialTyper.scala 
cp scalac-plugin-typer.xml classes/scalac-plugin.xml
(cd classes; jar cf ../facttyper.jar .)

