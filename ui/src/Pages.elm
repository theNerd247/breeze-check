module Pages exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Progress as Progress
import BreezeApi as BreezeApi
import ErrorMsg as Err
import EventInfo as EventInfo
import FindPeople as Find
import Html as Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class, for, style)
import Html.Events exposing (onClick)
import Nested exposing (modifyCmd)
import NewPerson as NewPerson
import Router as Router


type Msg
    = FindMsg Find.Msg
    | NewPersonMsg NewPerson.Msg
    | Continue
    | ErrorMsg Err.Msg
    | EventInfoMsg EventInfo.Msg
    | RouterMsg Router.Msg
    | NavbarMsg Navbar.State
    | AgreedToSafetyWaiver Bool


type alias Model =
    Router.HasRoutes
        (BreezeApi.HasBreezeApi
            (EventInfo.HasEventInfo
                (Find.HasFind
                    (NewPerson.HasNewPersons
                        { navbarState : Navbar.State
                        , aggreedToSafetyWaiver : Bool
                        }
                    )
                )
            )
        )


type alias Config =
    { pageRoute : Router.Route
    , nextPageRoute : Router.Route
    , pageTitle : String
    , pageView : Model -> Html Msg
    , showInNavbar : Bool
    }


type alias Pages =
    List Config


config : Config
config =
    { pageRoute = Router.Home
    , nextPageRoute = Router.Home
    , pageTitle = ""
    , pageView = always (Html.div [] [])
    , showInNavbar = False
    }


pageNotFound : Config
pageNotFound =
    { config
        | pageTitle = "404"
        , nextPageRoute = Router.Home
        , pageView =
            always <|
                div []
                    [ h3 [] [ text "The page you're looking for doesn't exist" ]
                    , continueButton False [ text "Home" ]
                    ]
    }


getCurrentConfig : Pages -> Model -> Config
getCurrentConfig pages mdl =
    List.filter (\x -> x.pageRoute == mdl.currentRoute) pages
        |> List.head
        |> Maybe.withDefault pageNotFound


withCurrentPage : Model -> Pages -> (Config -> a) -> a
withCurrentPage mdl pages f =
    f <| getCurrentConfig pages mdl


update : Msg -> Model -> Config -> ( Model, Cmd Msg )
update msg mdl cfg =
    case msg of
        FindMsg m ->
            Find.afterSearch m (Router.setRoute mdl Router.Selected) mdl
                |> Find.afterCheckIn m (Router.setRoute mdl Router.WaitingApproval)
                |> Find.afterCancel m (Router.setRoute mdl Router.Selected)
                |> Find.update m
                |> modifyCmd FindMsg

        Continue ->
            ( Router.setRoute mdl cfg.nextPageRoute, Cmd.none )

        NewPersonMsg m ->
            ( NewPerson.update m mdl, Cmd.none )

        ErrorMsg m ->
            ( Err.update m mdl, Cmd.none )

        RouterMsg m ->
            ( Router.update m mdl, Cmd.none )

        NavbarMsg state ->
            ( { mdl | navbarState = state }, Cmd.none )

        EventInfoMsg m ->
            modifyCmd EventInfoMsg <| EventInfo.update m mdl

        AgreedToSafetyWaiver b ->
            ( { mdl | aggreedToSafetyWaiver = b }, Cmd.none )


view : Model -> Config -> Html Msg
view mdl cfg =
    let
        errors =
            Html.map ErrorMsg <| Err.view mdl

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
                [ Grid.col [ Col.lg8 ]
                    [ cfg.pageView mdl
                    ]
                ]
    in
    Grid.containerFluid []
        [ errors
        , loading
        , page
        ]


navbar : Config -> Pages -> Model -> Html Msg
navbar cfg pages mdl =
    let
        navBarItem c =
            Navbar.itemLink
                [ onClick <|
                    RouterMsg <|
                        Router.SetRoute
                            c.pageRoute
                ]
                [ text <| c.pageTitle ]
    in
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand []
            [ h3 [] [ text <| cfg.pageTitle ] ]
        |> Navbar.items
            (pages |> List.filter .showInNavbar |> List.map navBarItem)
        |> Navbar.view mdl.navbarState


navbarSubscriptions : Model -> Sub Msg
navbarSubscriptions mdl =
    Navbar.subscriptions mdl.navbarState NavbarMsg


loadingBar : Html msg
loadingBar =
    Progress.progress
        [ Progress.value 100
        , Progress.animated
        ]


continueButton : Bool -> List (Html Msg) -> Html Msg
continueButton disabled buttonText =
    if disabled then
        Html.div [] []
    else
        Grid.row [ Row.centerXs, Row.attrs [ class "align-items-end" ] ]
            [ Grid.col [ Col.xs12 ]
                [ div [ class "text-center", class "align-text-bottom" ]
                    [ Button.button
                        [ Button.onClick Continue
                        , Button.outlineSuccess
                        , Button.disabled disabled
                        ]
                        buttonText
                    ]
                ]
            ]


goToPageButton : Router.Route -> List (Html Msg) -> Html Msg
goToPageButton r buttonText =
    Button.button
        [ Button.onClick <| RouterMsg <| Router.SetRoute r
        , Button.outlinePrimary
        ]
        buttonText


checkInButton : Bool -> Html Msg
checkInButton disabled =
    if disabled then
        Html.div [] []
    else
        Grid.row [ Row.centerXs, Row.attrs [ class "align-items-end" ] ]
            [ Grid.col [ Col.xs12 ]
                [ div [ class "text-center", class "align-text-bottom" ]
                    [ Button.button
                        [ Button.onClick <| FindMsg Find.CheckInClick
                        , Button.outlineSuccess
                        , Button.disabled disabled
                        ]
                        [ text "Check-in" ]
                    ]
                ]
            ]
