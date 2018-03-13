module Router exposing (..)

import Html exposing (Html)
import Navigation as Nav
import RouteUrl as Url
import RouteUrl.Builder as Builder
import UrlParser exposing ((<?>), customParam, map, oneOf, parsePath, top)


type Route
    = Home
    | Search
    | Selected
    | NewPersons
    | Cart
    | Photo
    | Safety
    | WaitingApproval


type Msg
    = SetRoute Route



--NOTE: use the setRoute function to force proper routing. Don't set this
--directly!!!!!


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


update : Msg -> HasRoutes m -> HasRoutes m
update msg mdl =
    case msg of
        SetRoute r ->
            setRoute mdl r


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

        Cart ->
            "cart"

        Safety ->
            "safety"

        Home ->
            "home"

        NewPersons ->
            "newfamily"


guardRoute : Route -> Route -> Route
guardRoute old new =
    case new of
        Photo ->
            case old of
                Safety ->
                    Photo

                _ ->
                    Home

        _ ->
            new


setRoute : HasRoutes m -> Route -> HasRoutes m
setRoute mdl r =
    { mdl | currentRoute = guardRoute mdl.currentRoute r }


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
                    Home

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

                        "cart" ->
                            Cart

                        "safety" ->
                            Safety

                        "home" ->
                            Home

                        _ ->
                            Home
    in
    (top <?> customParam "page" pg)
        |> flip parsePath l
        |> Maybe.map (List.singleton << SetRoute)
        |> Maybe.withDefault [ SetRoute Home ]
