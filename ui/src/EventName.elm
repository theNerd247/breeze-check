module EventName exposing (..)

import BreezeApi as BreezeApi
import Data exposing (EventName)


type Msg
    = GetEventName
    | EventNameResult (BreezeApi.Msg EventName)


type alias HasEventName m =
    BreezeApi.HasBreezeApi { m | eventName : String }


update : Msg -> HasEventName m -> ( HasEventName m, Cmd Msg )
update msg mdl =
    case msg of
        GetEventName ->
            BreezeApi.eventInfo EventNameResult mdl

        EventNameResult r ->
            BreezeApi.update eventNameResult r mdl


eventNameResult : EventName -> HasEventName m -> ( HasEventName m, Cmd Msg )
eventNameResult n mdl =
    ( { mdl | eventName = n }, Cmd.none )
