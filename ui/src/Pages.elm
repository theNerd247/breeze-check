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
    | NewPerson NewPerson.Msg
    | Continue
    | Error Err.Msg
    | RouterMsg Router.Msg
    | NavbarMsg Navbar.State


type alias Model =
    Router.HasRoutes
        (BreezeApi.HasBreezeApi
            (EventInfo.HasEventInfo
                (Find.HasFind
                    (NewPerson.HasNewPersons
                        { navbarState : Navbar.State
                        }
                    )
                )
            )
        )


type alias Config =
    { pageRoute : Router.Route
    , nextPageRoute : Router.Route
    , pageTitle : String
    , view : Model -> Html Msg
    , showContinue : Bool
    }


type alias Pages =
    List Config


config : Config
config =
    { pageRoute = Router.Home
    , nextPageRoute = Router.Home
    , pageTitle = ""
    , view = always (Html.div [] [])
    , showContinue = False
    }


pageRoute : Router.Route -> Config -> Config
pageRoute r mdl =
    { mdl | pageRoute = r }


nextPage : Router.Route -> Config -> Config
nextPage r mdl =
    { mdl | nextPageRoute = r }


title : String -> Config -> Config
title r mdl =
    { mdl | pageTitle = r }


pageView : (Model -> Html Msg) -> Config -> Config
pageView r mdl =
    { mdl | view = r }


showContinue : Bool -> Config -> Config
showContinue b mdl =
    { mdl | showContinue = b }


pageNotFound : Config
pageNotFound =
    config
        |> title "404"
        |> showContinue True
        |> nextPage Router.Home
        |> pageView (always <| h3 [] [ text "The page you're looking for doesn't exist" ])


getCurrentConfig : Pages -> Model -> Config
getCurrentConfig pages mdl =
    List.filter (\x -> x.pageRoute == mdl.currentRoute) pages
        |> List.head
        |> Maybe.withDefault pageNotFound


withCurrentPage : Model -> (Config -> a) -> Pages -> a
withCurrentPage mdl f pages =
    f <| getCurrentConfig pages mdl


update : Msg -> Model -> Config -> ( Model, Cmd Msg )
update msg mdl cfg =
    case msg of
        FindMsg msg ->
            modifyCmd FindMsg <| Find.update msg mdl

        Continue ->
            ( Router.setRoute mdl cfg.nextPageRoute, Cmd.none )

        NewPerson msg ->
            ( NewPerson.update msg mdl, Cmd.none )

        Error msg ->
            ( Err.update msg mdl, Cmd.none )

        RouterMsg msg ->
            ( Router.update msg mdl, Cmd.none )

        NavbarMsg state ->
            ( { mdl | navbarState = state }, Cmd.none )


view : Model -> Config -> Html Msg
view mdl cfg =
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
                [ Grid.col [ Col.lg8 ]
                    [ cfg.view mdl
                    ]
                ]
    in
    Grid.containerFluid []
        [ errors
        , loading
        , page
        ]


navbar : Model -> Pages -> Config -> Html Msg
navbar mdl pages cfg =
    let
        navBarItem c =
            Navbar.itemLink
                [ onClick <|
                    RouterMsg <|
                        Router.SetRoute
                            c.pageRoute
                ]
                [ text "Home" ]
    in
    Navbar.config NavbarMsg
        |> Navbar.brand []
            [ h3 [] [ text <| cfg.pageTitle ] ]
        |> Navbar.items
            (pages |> List.map navBarItem)
        |> Navbar.view mdl.navbarState


loadingBar : Html msg
loadingBar =
    Progress.progress
        [ Progress.value 100
        , Progress.animated
        ]


continueButton : Bool -> String -> Html Msg
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
                        [ text buttonText
                        ]
                    ]
                ]
            ]
