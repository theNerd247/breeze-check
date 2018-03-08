module BreezeApi exposing (..)

import Data as Data
import ErrorMsg as Err
import Http as Http exposing (Error(..))
import Json.Decode as Decode
import Nested exposing (modifyCmd)
import Result
import Time exposing (second)


type alias Response a =
    Result Http.Error (Result Data.BreezeException a)


type alias HasLoadingStatus m =
    { m | loadingStatus : Bool }


type alias HasBreezeApi m =
    Err.HasErrors (HasLoadingStatus m)


type Msg a
    = Recieved (Response a)


update : (a -> HasBreezeApi m -> ( HasBreezeApi m, Cmd msg )) -> Msg a -> HasBreezeApi m -> ( HasBreezeApi m, Cmd msg )
update f (Recieved r) mdl =
    fromResponse r
        |> fromResult
            (\e -> ( Err.newError e mdl, Cmd.none ))
            (flip f mdl)


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
                , expect = Http.expectJson (Data.withBreezeErrDecoder decoder)
                , timeout = Just <| 8 * second
                , withCredentials = False
                }
    in
    ( { mdl | loadingStatus = True }, Http.send Recieved req )


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
            Data.decodePersons
            f
            mdl


checkIn f ppl mdl =
    sendPost
        "checkin"
        (Http.jsonBody <| Data.encodePersons ppl)
        Data.decodeGroupId
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
        Data.decodeEventName
        f
        mdl


getCheckInGroup f gid mdl =
    sendGet
        ("getgroup?groupid=" ++ toString gid)
        Data.decodePersons
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
