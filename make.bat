call elm make Main.elm --output=StrikeGenUncompiled.js
java -jar \Java\closure-compiler\compiler.jar -O ADVANCED --js StrikeGenUncompiled.js --js_output_file StrikeGen.js
