module Router exposing (..)

import Html exposing (Html)
import Navigation as Nav
import Pages as Pages exposing (HasPageWrapper, pageWrapper)
import Pages.PhotoPage as PhotoPage exposing (HasPhotoPage)
import Pages.SearchPage as SearchPage exposing (HasSearchPage)
import Pages.SelectPage as SelectPage exposing (HasSelectPage)
import Pages.WaitingApprovalPage as WaitingApprovalPage exposing (HasWaitingApprovalPage)
import RouteUrl as Url
import UrlParser exposing (map, oneOf, parsePath, s)


type Route
    = Search
    | Selected
    | Photo
    | WaitingApproval


type Msg
    = SetRoute Route
    | PageWrapperMsg Pages.Msg
    | SearchPageMsg SearchPage.Msg
    | SelectPageMsg SelectPage.Msg
    | PhotoPageMsg PhotoPage.Msg
    | WaitingApprovalPageMsg WaitingApprovalPage.Msg


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


type alias HasPageRouter m =
    HasRoutes (HasPageWrapper (HasSearchPage (HasSelectPage (HasPhotoPage (HasWaitingApprovalPage m)))))


viewPage : HasPageRouter m -> Html Msg
viewPage mdl =
    pageWrapper mdl PageWrapperMsg <|
        case mdl.currentRoute of
            Search ->
                Html.map SearchPageMsg <| SearchPage.view mdl

            Selected ->
                Html.map SelectPageMsg <| SelectPage.view mdl

            Photo ->
                Html.map PhotoPageMsg <| PhotoPage.view mdl

            WaitingApproval ->
                Html.map WaitingApprovalPageMsg <| WaitingApprovalPage.view mdl


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
        |> Maybe.withDefault []
