module EventInfo exposing (..)

import BreezeApi as BreezeApi
import Data exposing (EventInfo)


type Msg
    = GetEventInfo
    | EventInfoResult (BreezeApi.Msg EventInfo)


type alias HasEventInfo m =
    BreezeApi.HasBreezeApi { m | eventInfo : EventInfo }


update : Msg -> HasEventInfo m -> ( HasEventInfo m, Cmd Msg )
update msg mdl =
    case msg of
        GetEventInfo ->
            BreezeApi.eventInfo EventInfoResult mdl

        EventInfoResult r ->
            BreezeApi.update eventNameResult r mdl


eventNameResult : EventInfo -> HasEventInfo m -> ( HasEventInfo m, Cmd Msg )
eventNameResult n mdl =
    ( { mdl | eventInfo = n }, Cmd.none )
