module Router exposing (..)

import Html exposing (Html)
import Navigation as Nav
import RouteUrl as Url
import UrlParser exposing (map, oneOf, parsePath, s)


type Route
    = Search
    | Selected
    | Photo
    | WaitingApproval


type Msg
    = SetRoute Route


type alias HasRoutes m =
    { m | currentRoute : Route }



-- | an alias for a normal program type


type alias Prog msg model =
    { init : ( model, Cmd msg ), update : msg -> model -> ( model, Cmd msg ), subscriptions : model -> Sub msg, view : model -> Html msg }


mainWithRouter : Prog msg (HasRoutes mdl) -> (Msg -> msg) -> Url.RouteUrlProgram Never (HasRoutes mdl) msg
mainWithRouter prog f =
    Url.program
        { delta2url = delta2url
        , location2messages = List.map f << location2messages
        , init = prog.init
        , update = prog.update
        , subscriptions = prog.subscriptions
        , view = prog.view
        }


update : Msg -> HasRoutes m -> ( HasRoutes m, Cmd Msg )
update msg mdl =
    case msg of
        SetRoute r ->
            ( { mdl | currentRoute = r }, Cmd.none )


routeName : Route -> String
routeName r =
    case r of
        Search ->
            "search"

        Selected ->
            "selected"

        Photo ->
            "photo"

        WaitingApproval ->
            "waiting"


delta2url : HasRoutes m -> HasRoutes m -> Maybe Url.UrlChange
delta2url old new =
    Just { url = routeName new.currentRoute, entry = Url.NewEntry }


location2messages : Nav.Location -> List Msg
location2messages l =
    oneOf
        [ map Search <| s "search"
        , map Selected <| s "selected"
        , map Photo <| s "photo"
        , map WaitingApproval <| s "waiting"
        ]
        |> flip parsePath l
        |> Maybe.map (List.singleton << SetRoute)
        |> Maybe.withDefault [ SetRoute Search ]
