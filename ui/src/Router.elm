module Router exposing (..)

import Html exposing (Html)
import Navigation as Nav
import RouteUrl as Url
import RouteUrl.Builder as Builder
import UrlParser exposing ((<?>), customParam, map, oneOf, parsePath, top)


type Route
    = Search
    | Selected
    | Photo
    | WaitingApproval
    | Review
    | Home


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

        Review ->
            "review"

        Home ->
            "home"


setRoute : HasRoutes m -> Route -> HasRoutes m
setRoute mdl r =
    { mdl | currentRoute = r }


delta2url : HasRoutes m -> HasRoutes m -> Maybe Url.UrlChange
delta2url old new =
    Builder.builder
        |> Builder.newEntry
        |> Builder.replaceQuery [ ( "page", routeName new.currentRoute ) ]
        |> Builder.toUrlChange
        |> Just


location2messages : Nav.Location -> List Msg
location2messages l =
    let
        pg r =
            case r of
                Nothing ->
                    Search

                Just s ->
                    case s of
                        "search" ->
                            Search

                        "selected" ->
                            Selected

                        "photo" ->
                            Photo

                        "waiting" ->
                            WaitingApproval

                        "review" ->
                            Review

                        "home" ->
                            Home

                        _ ->
                            Search
    in
    (top <?> customParam "page" pg)
        |> flip parsePath l
        |> Maybe.map (List.singleton << SetRoute)
        |> Maybe.withDefault [ SetRoute Search ]
