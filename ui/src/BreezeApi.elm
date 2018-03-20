module BreezeApi exposing (..)

import Data as Data
import ErrorMsg as Err
import Http as Http exposing (Error(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Nested exposing (modifyCmd)
import Result
import Time exposing (second)


type alias Response a =
    Result Http.Error (Result Data.BreezeException a)


type LoadingStatus
    = Waiting
    | DoneOk
    | DoneError


type alias HasLoadingStatus m =
    { m | loadingStatus : LoadingStatus }


type alias HasBreezeApi m =
    Err.HasErrors (HasLoadingStatus m)


type Msg a
    = Recieved (Response a)


initLoadingStatus : LoadingStatus
initLoadingStatus =
    DoneOk


onSuccess : Msg a -> (a -> m -> m) -> m -> m
onSuccess (Recieved msg) f m =
    case msg of
        Ok (Ok a) ->
            f a m

        _ ->
            m


update : (a -> HasBreezeApi m -> ( HasBreezeApi m, Cmd msg )) -> Msg a -> HasBreezeApi m -> ( HasBreezeApi m, Cmd msg )
update f (Recieved r) mdl =
    fromResponse r
        |> fromResult
            (\e -> ( Err.newError e { mdl | loadingStatus = DoneError }, Cmd.none ))
            (flip f { mdl | loadingStatus = DoneOk })


withBreezeErrDecoder : Decode.Decoder a -> Decode.Decoder (Result Data.BreezeException a)
withBreezeErrDecoder d =
    Decode.oneOf
        [ Decode.map Err Data.decodeBreezeException
        , Decode.map Ok d
        , Decode.fail "Something went wrong when fetching data"
        ]


sendApiGetRequest :
    String --  method
    -> Http.Body -- bodywithBreezeErrDecoder
    -> String -- path relative to base
    -> Decode.Decoder a -- decoder to use for the data
    -> HasBreezeApi m
    -> ( HasBreezeApi m, Cmd (Msg a) )
sendApiGetRequest meth bdy path decoder mdl =
    let
        req =
            Http.request
                { method = meth
                , headers = []
                , url = path
                , body = bdy
                , expect = Http.expectJson (withBreezeErrDecoder decoder)
                , timeout = Just <| 8 * second
                , withCredentials = False
                }
    in
    ( { mdl | loadingStatus = Waiting }, Http.send Recieved req )


sendGet p dec f mdl =
    modifyCmd f <| sendApiGetRequest "GET" Http.emptyBody p dec mdl


sendPost path bdy dec f mdl =
    modifyCmd f <| sendApiGetRequest "POST" bdy path dec mdl


findPeople f lastName mdl =
    if String.isEmpty lastName then
        ( mdl, Cmd.none )
    else
        sendGet
            ("findperson?lastname=" ++ lastName)
            (Decode.list Data.decodePerson)
            f
            mdl


checkIn f ppl mdl =
    sendPost
        "checkin"
        (Http.jsonBody <| Encode.list <| List.map Data.encodePerson ppl)
        Decode.int
        f
        mdl


cancelCheckin f mgid mdl =
    case mgid of
        Nothing ->
            ( mdl, Cmd.none )

        Just gid ->
            sendGet
                ("cancel?groupid=" ++ toString gid)
                Decode.bool
                f
                mdl


eventInfo f mdl =
    sendGet
        "eventinfo"
        Data.decodeEventInfo
        f
        mdl


getCheckInGroup f gid mdl =
    sendGet
        ("getgroup?groupid=" ++ toString gid)
        (Decode.list Data.decodePerson)
        f
        mdl


approveCheckIn f gid mdl =
    sendGet
        ("approve?groupid=" ++ toString gid)
        Decode.bool
        f
        mdl


fromResult : (a -> c) -> (b -> c) -> Result.Result a b -> c
fromResult f g r =
    case r of
        Err a ->
            f a

        Ok b ->
            g b


getEventList f mdl =
    sendGet "eventList" (Decode.list Data.decodeEventInfo) f mdl


setEventInfo eid f mdl =
    sendPost "eventinfo" (Http.jsonBody <| Encode.string eid) Data.decodeEventInfo f mdl


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
