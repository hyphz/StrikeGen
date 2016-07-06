call dist.bat
"\program files\winzip\wzzip" -a -r -o -P -ex web\StrikeGen.zip dist
xcopy dist web /s /y
copy SiteIndex.html web\index.html
cd web
git add .
git commit -m "Web image update."
git push
