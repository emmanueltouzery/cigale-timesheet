import Http
import Json

-- do i really have an array, or is it maybe a hash?
serverJson : Signal [JsonValue]
serverJson = let jsonTxtSig = lift respStr (Http.sendGet (constant "/timesheet/2013-06-01"))
             in lift getJsonArray jsonTxtSig

getJsonArray : String -> [JsonValue]
getJsonArray jsonTxt = case Json.fromString jsonTxt of
                        Just x -> x
                        _ -> [Number 1, Number 2, Number 3]

respStr : Response String -> String
respStr resp = case resp of
    Success string -> string
    _ -> ""

showResults : String -> Element
showResults json = flow down [plainText "Hour", plainText json, plainText "Description", plainText "Extra info"]

main = lift (showResults . show . length) serverJson
