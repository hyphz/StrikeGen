# StrikeGen
Character Generator for the Strike RPG.

Test deployment available here: http://hyphz.github.io/StrikeGen.html

Jim McGarva has given permission for the use of the names of backgrounds, origins, kits, kit advances, feats,
classes and roles from the main book and expansions, and for implementation of their computable effects on a
character (including relationships to other items by name), and for reproduction of the text of powers
from the core book, but *not* for reproduction of the full text of powers from outside the core book.

## Building

Download, download node.js and use it to install Elm ( http://elm-lang.org/ ), then run *make.bat* from
the node.js command prompt. If you're not on Windows, do whatever your equivalent is and then run
the same command that's in make.bat . Then, open StrikeGen.html in a browser.

To build a deployable version, run *dist.bat* and it'll be in the dist directory.

Running using *elm reactor* will omit the style sheet and make the file menu and alerts not work. It
still might be useful for debugging purposes.

## Add-on Credits

* Icons made by Freepik from www.flaticon.com: range (*bow-and-arrow*), attack (*crossed-swords*), melee (*hammer*), damage (*drop-silhouette*), role (*multiple-users-silhouette*), move (*runer-silhouette-running-fast*), reaction (*storm-pronostic*), area (*explosion*).

* Icon made by Vaadin from www.flaticon.com: circle (*circle*).

* *download-js" by DANML.

* *lz-string* by Pieroxy.

* Google Material Design icons supplied by Google under the Apache License, integrated into elm by Theseamau5.
