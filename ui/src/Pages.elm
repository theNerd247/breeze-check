module Pages exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Popover as Popover
import Bootstrap.Progress as Progress
import BreezeApi as BreezeApi
import Dict as Dict
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
    | ErrorMsg Err.Msg
    | EventInfoMsg EventInfo.Msg
    | RouterMsg Router.Msg
    | NavbarMsg Navbar.State
    | AgreedToSafetyWaiver Bool
    | PopoverMsg Popover.State


type alias Model =
    Router.HasRoutes
        (BreezeApi.HasBreezeApi
            (EventInfo.HasEventInfo
                (Find.HasFind
                    (NewPerson.HasNewPersons
                        { navbarState : Navbar.State
                        , aggreedToSafetyWaiver : Bool
                        , popoverState : Popover.State

                        --, needsNewPersons : Bool
                        }
                    )
                )
            )
        )


type alias Config =
    { pageRoute : Router.Route
    , pageTitle : String
    , pageView : Model -> Html Msg
    , showInNavbar : Bool
    }


type alias Pages =
    List Config


config : Config
config =
    { pageRoute = Router.Home
    , pageTitle = ""
    , pageView = always (Html.div [] [])
    , showInNavbar = False
    }


pageNotFound : Config
pageNotFound =
    { config
        | pageTitle = "404"
        , pageView =
            always <|
                div []
                    [ h3 [] [ text "The page you're looking for doesn't exist" ]
                    , continueButton False Router.Home [ text "Home" ]
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

        NewPersonMsg m ->
            let
                newPpl =
                    mdl.newPersons
                        |> Dict.values
                        |> List.map (\p -> ( p.pid, p ))
                        |> Dict.fromList

                --the order of union matters. if the user goes
                --back to edit the persons then we want those
                --items to update in the rest of the model
                merged =
                    Dict.union newPpl mdl.waitingCheckIn
            in
            ( mdl
                |> NewPerson.afterCreateNewAttendees m
                    (Router.setRoute
                        { mdl
                            | waitingCheckIn = merged
                            , foundPeople = merged
                            , searchLastName = ""
                            , personNotFound = Nothing
                        }
                        Router.Selected
                    )
                |> NewPerson.update m
            , Cmd.none
            )

        ErrorMsg m ->
            ( Err.update m mdl, Cmd.none )

        RouterMsg m ->
            ( Router.update m mdl, Cmd.none )

        NavbarMsg state ->
            ( { mdl | navbarState = state }, Cmd.none )

        PopoverMsg state ->
            ( { mdl | popoverState = state }, Cmd.none )

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
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs12 ]
                    [ if mdl.loadingStatus == BreezeApi.Waiting then
                        loadingBar
                      else
                        text ""
                    ]
                ]

        page =
            Grid.row [ Row.centerXs, Row.attrs [ class "h-100" ] ]
                [ Grid.col [ Col.xs12 ]
                    [ cfg.pageView mdl
                    ]
                ]
    in
    div [ class "d-flex h-100 flex-column justify-content-center" ]
        --[ Row.attrs [ Flex.col, Size.h100, Flex.alignItemsCenter ], Row.leftXs ]
        [ div [ class "order-1 w-100 grow-auto" ] [ errors ]
        , div [ class "order-2 w-100 grow-auto" ] [ loading ]
        , div [ class "order-3 w-100 grow-1 h-100" ] [ page ]
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
        |> Navbar.attrs [ class "order-1" ]
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
        , Progress.wrapperAttrs [ style [ ( "height", "20px" ) ] ]
        ]


continueButton : Bool -> Router.Route -> List (Html Msg) -> Html Msg
continueButton disabled r buttonText =
    Button.button
        [ if disabled then
            Button.attrs []
          else
            Button.onClick <| RouterMsg <| Router.SetRoute r
        , if disabled then
            Button.outlineSecondary
          else
            Button.outlineSuccess
        , Button.disabled disabled
        ]
        buttonText


goToPageButton : Router.Route -> List (Html Msg) -> Html Msg
goToPageButton r buttonText =
    Button.button
        [ Button.onClick <| RouterMsg <| Router.SetRoute r
        , Button.outlinePrimary
        ]
        buttonText


checkInButton : Bool -> Html Msg
checkInButton disabled =
    Button.button
        [ if disabled then
            Button.attrs []
          else
            Button.onClick <| FindMsg Find.CheckInClick
        , if disabled then
            Button.outlineSecondary
          else
            Button.outlineSuccess
        , Button.disabled disabled
        ]
        [ text "Check-in" ]


pageWrapper : String -> List (Html msg) -> Html msg
pageWrapper attrs =
    div [ class <| "d-flex flex-column h-100 align-items-center " ++ attrs ]


navButtons : Router.Route -> Bool -> Router.Route -> Html Msg
navButtons rb disabled rc =
    let
        continueCol =
            continueButton disabled
    in
    div [ class "grow-6 d-flex flex-row justify-content-center w-100" ]
        [ div [ class "grow-auto px-3" ] [ goToPageButton rb [ text "Back" ] ]
        , div [ class "grow-auto px-3" ] [ continueButton disabled rc [ text "Next" ] ]
        ]
