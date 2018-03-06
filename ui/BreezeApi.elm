module BreezeApi exposing (..)

import Data as Data
import Http as Http exposing (Error(..))
import Json.Decode as Decode
import Result
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


findPeople : (Response (List Data.Person) -> msg) -> String -> Cmd msg
findPeople f lastName =
    if String.isEmpty lastName then
        Cmd.none
    else
        sendGet
            ("findperson?lastname=" ++ lastName)
            (Data.withBreezeErrDecoder Data.decodePersons)
            f


checkIn : (Response Data.GroupId -> msg) -> List Data.Person -> Cmd msg
checkIn f ppl =
    sendPost
        "checkin"
        (Http.jsonBody <| Data.encodePersons ppl)
        (Data.withBreezeErrDecoder Data.decodeGroupId)
        f


cancelCheckin : (Response Bool -> msg) -> Maybe Int -> Cmd msg
cancelCheckin f mgid =
    case mgid of
        Nothing ->
            Cmd.none

        Just gid ->
            sendGet
                ("cancel?groupid=" ++ toString gid)
                (Data.withBreezeErrDecoder Decode.bool)
                f


eventInfo : (Response String -> msg) -> Cmd msg
eventInfo f =
    sendGet
        "eventinfo"
        (Data.withBreezeErrDecoder Data.decodeEventName)
        f


getCheckInGroup : Data.GroupId -> (Response (List Data.Person) -> msg) -> Cmd msg
getCheckInGroup gid f =
    sendGet
        ("getgroup?groupid=" ++ toString gid)
        (Data.withBreezeErrDecoder Data.decodePersons)
        f


approveCheckIn : Data.GroupId -> (Response Bool -> msg) -> Cmd msg
approveCheckIn gid f =
    sendGet
        ("approve?groupid=" ++ toString gid)
        (Data.withBreezeErrDecoder Decode.bool)
        f


fromResult : (a -> c) -> (b -> c) -> Result.Result a b -> c
fromResult f g r =
    case r of
        Err a ->
            f a

        Ok b ->
            g b


fromResponse :
    Response a
    -> Result.Result String a
fromResponse resp =
    let
        writeErrorString e =
            case Debug.log ("Http Error: " ++ toString e) e of
                BadPayload _ _ ->
                    """
                    Some thing went wrong in getting data from the
                    server. You may need to delete your browser cache and refresh the
                    page.
                    """

                _ ->
                    "You have a network error"
    in
    Result.mapError writeErrorString resp
        |> Result.andThen (Result.mapError .breezeErr)
