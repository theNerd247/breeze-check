module BreezeApi exposing (..)

import Data as Data
import Http as Http
import Json.Decode as Decode
import Time exposing (second)


type alias Response a =
    Result Http.Error (Result Data.BreezeException a)


sendApiGetRequest :
    String --  method
    -> Http.Body -- body
    -> String -- path relative to base
    -> Decode.Decoder a -- decoder to use for the data
    ->
        (Result Http.Error a -- conversion from recieved data to a message
         -> msg
        )
    -> Cmd msg
sendApiGetRequest meth bdy path decoder f =
    let
        req =
            Http.request
                { method = meth
                , headers = []
                , url = path
                , body = bdy
                , expect = Http.expectJson decoder
                , timeout = Just <| 8 * second
                , withCredentials = False
                }
    in
    Http.send f req


sendGet :
    String
    -> Decode.Decoder a
    ->
        (Result Http.Error a
         -> msg
        )
    -> Cmd msg
sendGet =
    sendApiGetRequest "GET" Http.emptyBody


sendPost :
    String
    -> Http.Body
    -> Decode.Decoder a
    ->
        (Result Http.Error a
         -> msg
        )
    -> Cmd msg
sendPost path bdy =
    sendApiGetRequest "POST" bdy path


findPeople : String -> (Response (List Data.Person) -> msg) -> Cmd msg
findPeople lastName f =
    if String.isEmpty lastName then
        Cmd.none
    else
        sendGet
            ("findperson?lastname=" ++ lastName)
            Data.decodePersons
            f


checkIn : List Data.Person -> (Response Data.GroupId -> msg) -> Cmd msg
checkIn ppl f =
    sendPost
        "checkin"
        (Http.jsonBody <| Data.encodePersonIds ppl)
        Data.decodeGroupId
        f


cancelCheckin : Maybe Int -> (Response Bool -> msg) -> Cmd msg
cancelCheckin mgid f =
    case mgid of
        Nothing ->
            Cmd.none

        Just gid ->
            sendGet
                ("cancel?groupid=" ++ toString gid)
                (Data.withBreezeErrDecoder Decode.bool)
                f
