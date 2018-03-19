module Router exposing (..)

import Data as Data
import Html exposing (Html)
import Navigation as Nav
import RouteUrl as Url
import RouteUrl.Builder as Builder
import UrlParser
    exposing
        ( (<?>)
        , customParam
        , map
        , oneOf
        , parsePath
        , stringParam
        , top
        )


type Route
    = Home
    | Search
    | Selected
    | NewPersons
    | EditFamilyInfo Data.LastName
    | Cart
    | Photo
    | Safety
    | WaitingApproval
    | PageNotFound


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


routeName : Route -> Builder.Builder -> Builder.Builder
routeName r =
    let
        pageQ t =
            Builder.addQuery "page" t
    in
    case r of
        Search ->
            pageQ "search"

        Selected ->
            pageQ "selected"

        Photo ->
            pageQ "photo"

        WaitingApproval ->
            pageQ "waiting"

        Cart ->
            pageQ "cart"

        Safety ->
            pageQ "safety"

        Home ->
            pageQ "home"

        NewPersons ->
            pageQ "newfamily"

        EditFamilyInfo lname ->
            pageQ "editfamily"
                >> Builder.addQuery "newlastname" lname

        PageNotFound ->
            pageQ "pagenotfound"


guardRoute : Route -> Route -> Route
guardRoute old new =
    let
        hasPrior x y =
            if old == x then
                y
            else
                old
    in
    case new of
        Photo ->
            hasPrior Cart Photo

        Safety ->
            hasPrior Photo Safety

        WaitingApproval ->
            hasPrior Safety WaitingApproval

        _ ->
            new


setRoute : HasRoutes m -> Route -> HasRoutes m
setRoute mdl r =
    { mdl | currentRoute = guardRoute mdl.currentRoute r }


delta2url : HasRoutes m -> HasRoutes m -> Maybe Url.UrlChange
delta2url old new =
    Builder.builder
        |> Builder.newEntry
        |> Builder.replaceQuery []
        |> routeName (guardRoute old.currentRoute new.currentRoute)
        |> Builder.toUrlChange
        |> Just


location2messages : Nav.Location -> List Msg
location2messages l =
    let
        pg t r =
            customParam "page" <|
                Maybe.andThen
                    (\p ->
                        if t == p then
                            Just r
                        else
                            Nothing
                    )

        search =
            top <?> pg "search" Search

        selected =
            top <?> pg "selected" Selected

        photo =
            top <?> pg "photo" Photo

        waiting =
            top <?> pg "waiting" WaitingApproval

        safety =
            top <?> pg "safety" Safety

        home =
            top <?> pg "home" Home

        newfamily =
            top <?> pg "newfamily" NewPersons

        cart =
            top <?> pg "cart" Cart

        editPersons =
            let
                combineR mr ms =
                    Maybe.andThen (\f -> Maybe.map (\s -> f s) ms) mr
            in
            map combineR <| top <?> pg "editfamilyinfo" EditFamilyInfo <?> stringParam "newlastname"

        resolveMaybe x =
            case x of
                Nothing ->
                    Nothing

                Just x ->
                    x
    in
    oneOf
        [ home
        , search
        , selected
        , newfamily
        , cart
        , photo
        , safety
        , waiting
        , editPersons
        ]
        |> flip parsePath l
        |> resolveMaybe
        |> Maybe.map (List.singleton << SetRoute)
        |> Maybe.withDefault [ SetRoute PageNotFound ]
