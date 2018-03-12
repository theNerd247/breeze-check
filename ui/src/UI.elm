module UI exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Progress as Progress
import BreezeApi as BreezeApi
import Dict as Dict
import ErrorMsg as Err
import EventInfo as Event
import FindPeople as Find
import Html as Html
    exposing
        ( Html
        , body
        , div
        , h1
        , h2
        , h3
        , h4
        , header
        , p
        , text
        )
import Html.Attributes exposing (class, for, style)
import Html.Events exposing (onClick)
import Nested exposing (modifyCmd, modifyMdl)
import NewPerson as NewPerson
import Pages.CartPage as CartPage
import Pages.HomePage as HomePage
import Pages.PhotoPage as PhotoPage
import Pages.SearchPage as SearchPage
import Pages.SelectPage as SelectPage
import Pages.WaitingApprovalPage as WaitingApprovalPage
import Person as Person
import Router as Router exposing (HasRoutes, mainWithRouter)


type alias Model =
    HasRoutes
        (BreezeApi.HasBreezeApi
            (Event.HasEventInfo
                (Find.HasFind
                    (NewPerson.HasNewPersons
                        { navbarState : Navbar.State
                        }
                    )
                )
            )
        )


type Msg
    = RouterMsg Router.Msg
    | Error Err.Msg
    | EventName Event.Msg
    | SearchPage SearchPage.Msg
    | SelectPage SelectPage.Msg
    | PhotoPage PhotoPage.Msg
    | WaitingApprovalPage WaitingApprovalPage.Msg
    | CartPage CartPage.Msg
    | NavbarMsg Navbar.State


main =
    --Html.program
    mainWithRouter
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
        RouterMsg


init : ( Model, Cmd Msg )
init =
    let
        ( navstate, navmsg ) =
            Navbar.initialState NavbarMsg

        ( mdl, eventMsg ) =
            modifyCmd EventName <| Event.update Event.GetEventInfo m

        m =
            { errors = []
            , loadingStatus = BreezeApi.initLoadingStatus
            , groupId = Nothing
            , foundPeople = Dict.empty
            , waitingCheckIn = Dict.empty
            , searchLastName = ""
            , eventInfo = { eid = "", ename = "" }
            , personNotFound = False
            , newPerson = Person.initPerson
            , newPersons = Dict.empty
            , currentRoute = Router.Home
            , navbarState = navstate
            }
    in
    ( mdl
    , Cmd.batch
        [ eventMsg
        , navmsg
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        RouterMsg msg ->
            modifyCmd RouterMsg <| Router.update msg mdl

        Error msg ->
            ( Err.update msg mdl, Cmd.none )

        EventName msg ->
            modifyCmd EventName <| Event.update msg mdl

        SearchPage msg ->
            modifyCmd SearchPage <| SearchPage.update msg mdl

        SelectPage msg ->
            modifyCmd SelectPage <| SelectPage.update msg mdl

        PhotoPage msg ->
            modifyCmd PhotoPage <| PhotoPage.update msg mdl

        WaitingApprovalPage msg ->
            modifyCmd WaitingApprovalPage <| WaitingApprovalPage.update msg mdl

        CartPage msg ->
            modifyCmd CartPage <| CartPage.update msg mdl

        NavbarMsg state ->
            ( { mdl | navbarState = state }, Cmd.none )


view : Model -> Html Msg
view mdl =
    let
        errors =
            Html.map Error <| Err.view mdl

        loading =
            if mdl.loadingStatus == BreezeApi.Waiting then
                Grid.row [ Row.centerXs ]
                    [ Grid.col [ Col.xs12 ]
                        [ loadingBar
                        ]
                    ]
            else
                div [] []

        page =
            Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ viewPage mdl
                    ]
                ]

        navbar =
            Navbar.config NavbarMsg
                |> Navbar.brand []
                    [ h3 [] [ text <| pageTitle mdl ] ]
                |> Navbar.items
                    --TODO: replace with home link
                    [ Navbar.itemLink [ onClick <| routeMsg Router.Home ] [ text "Home" ]
                    , Navbar.itemLink [ onClick <| routeMsg Router.Search ] [ text "Search" ]
                    , Navbar.itemLink [ onClick <| routeMsg Router.Cart ] [ text "Cart" ]
                    ]
                |> Navbar.view mdl.navbarState
    in
    body []
        [ header [] [ navbar ]
        , Grid.containerFluid []
            [ errors
            , loading
            , viewPage mdl
            ]
        ]


routeMsg : Router.Route -> Msg
routeMsg =
    RouterMsg << Router.SetRoute


pageTitle : Model -> String
pageTitle mdl =
    case mdl.currentRoute of
        Router.Search ->
            "Search"

        Router.Selected ->
            "Results"

        Router.Photo ->
            "Photo Waiver"

        Router.Safety ->
            "Safety Agreement"

        Router.WaitingApproval ->
            "Done"

        Router.Cart ->
            "Check-in Cart"

        Router.Home ->
            "Home"


viewPage : Model -> Html Msg
viewPage mdl =
    case mdl.currentRoute of
        Router.Search ->
            Html.map SearchPage <| SearchPage.view mdl

        Router.Selected ->
            Html.map SelectPage <| SelectPage.view mdl

        Router.Photo ->
            Html.map PhotoPage <| PhotoPage.view mdl

        Router.WaitingApproval ->
            Html.map WaitingApprovalPage <| WaitingApprovalPage.view mdl

        Router.Cart ->
            Html.map CartPage <| CartPage.view mdl

        Router.Home ->
            Html.map RouterMsg <| HomePage.view mdl.eventInfo.ename

        -- TODO: remove once we have handlers for all routes
        _ ->
            div [] []


loadingBar : Html msg
loadingBar =
    Progress.progress
        [ Progress.value 100
        , Progress.animated
        ]
