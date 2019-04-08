call elm make Main.elm --optimize --output=StrikeGenUncompiled.js
c:\java\jdk-11.0.1\bin\java -jar \Java\closure-compiler\compiler.jar -O ADVANCED --jscomp_off=checkVars --js StrikeGenUncompiled.js --js_output_file StrikeGen.js
