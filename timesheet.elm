import Http
import Json

-- do i really have an array, or is it maybe a hash?
serverJson : Signal [JsonValue]
serverJson = let jsonTxtSig = lift respStr (Http.sendGet (constant "/timesheet/2013-06-07"))
             in lift getJsonArray jsonTxtSig

getJsonArray : String -> [JsonValue]
getJsonArray jsonTxt = case Json.fromString jsonTxt of
                        Just (Array x) -> x
                        _ -> []

respStr : Response String -> String
respStr resp = case resp of
    Success string -> string
    Waiting -> "[1,2]"
    _ -> "[1]"

showResults : String -> Element
showResults json = flow down [plainText "Hour", plainText json, plainText "Description", plainText "Extra info"]

main = lift (showResults . show . length) serverJson
