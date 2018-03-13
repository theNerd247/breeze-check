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
    , pageView : Model -> Html Msg
    }


type alias Pages =
    List Config


config : Config
config =
    { pageRoute = Router.Home
    , nextPageRoute = Router.Home
    , pageTitle = ""
    , pageView = always (Html.div [] [])
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


withCurrentPage : Model -> (Config -> a) -> Pages -> a
withCurrentPage mdl f pages =
    f <| getCurrentConfig pages mdl


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update cfg msg mdl =
    case msg of
        FindMsg msg ->
            modifyCmd FindMsg <| Find.update msg mdl

        Continue ->
            ( Router.setRoute mdl cfg.nextPageRoute, Cmd.none )

        NewPersonMsg msg ->
            ( NewPerson.update msg mdl, Cmd.none )

        ErrorMsg msg ->
            ( Err.update msg mdl, Cmd.none )

        RouterMsg msg ->
            ( Router.update msg mdl, Cmd.none )

        NavbarMsg state ->
            ( { mdl | navbarState = state }, Cmd.none )


view : Config -> Model -> Html Msg
view cfg mdl =
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
