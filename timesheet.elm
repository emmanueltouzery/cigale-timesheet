import Http

serverJson : Signal String
serverJson = lift respStr (Http.sendGet (constant "/timesheet/2013-06-07"))

respStr : Response String -> String
respStr resp = case resp of
    Success string -> string
    _ -> ""

showResults : String -> Element
showResults json = flow down [plainText "Hour", plainText json, plainText "Description", plainText "Extra info"]

main = lift showResults serverJson
