call dist.bat
cd dist
"\program files\winzip\wzzip" -a -r -o -P -ex ..\web\StrikeGen.zip *.*
cd..
xcopy dist web /s /y
copy StrikeGenWithTracking.html web\StrikeGen.html
copy SiteIndex.html web\index.html
cd web
git add .
git commit -m "Web image update."
git push
