port module Ports exposing ( .. )



port download : (String, String) -> Cmd x
port saveURL : String -> Cmd x
port dbLoaded : Int -> Cmd x
port doUpload : Int -> Cmd x
port alert : String -> Cmd x
port resetFileMenu : Int -> Cmd x


port loadJson : (String -> x) -> Sub x
