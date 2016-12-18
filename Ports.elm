port module Ports exposing (..)

-- OUTGOING


port download : ( String, String ) -> Cmd x



-- Send the string as a file with download.js .


port saveURL : String -> Cmd x



-- Pack the string as a parameter and reload the page.


port dbLoaded : Int -> Cmd x



-- Junk parameter. Called when loading is done to tell the JS that
-- it's safe to send us the JSON from the page parameter.


port doUpload : Int -> Cmd x



-- Junk parameter. Tells JS to open a file and send it with loadJson.


port alert : String -> Cmd x



-- Display a JS error box.


port resetFileMenu : Int -> Cmd x



-- Set the file menu selected item back to the header.


port doPrint : Int -> Cmd x



-- INCOMING


port loadJson : (String -> x) -> Sub x



-- Load a character from a JSON specification.
