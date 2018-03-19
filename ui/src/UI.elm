module UI exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Popover as Popover
import Bootstrap.Utilities.Flex as Flex
import BreezeApi as BreezeApi
import EventInfo as EventInfo
import Html exposing (div)
import Html.Attributes exposing (class, style)
import Nested exposing (modifyCmd)
import Pages exposing (..)
import Pages.CartPage as CartPage
import Pages.EditFamilyInfoPage as EditFamilyInfoPage
import Pages.HomePage as HomePage
import Pages.NewPersonsPage as NewPersonsPage
import Pages.PhotoPage as PhotoPage
import Pages.SafetyPage as SafetyPage
import Pages.SearchPage as SearchPage
import Pages.SelectPage as SelectPage
import Pages.WaitingApprovalPage as WaitingApprovalPage
import Person exposing (initPerson, initPersons)
import Router as Router exposing (mainWithRouter)


pages : Pages.Pages
pages =
    [ HomePage.config
    , SearchPage.config
    , SelectPage.config
    , NewPersonsPage.config
    , CartPage.config
    , PhotoPage.config
    , SafetyPage.config
    , WaitingApprovalPage.config
    , EditFamilyInfoPage.config
    ]


main =
    mainWithRouter uiProg RouterMsg


uiProg =
    { init = init
    , update = mainUpdate
    , view = mainView
    , subscriptions = navbarSubscriptions
    }


init : ( Model, Cmd Msg )
init =
    let
        ( navstate, navmsg ) =
            Navbar.initialState NavbarMsg

        ( mdl, eventMsg ) =
            modifyCmd EventInfoMsg <| EventInfo.update EventInfo.GetEventInfo m

        m =
            { errors = []
            , loadingStatus = BreezeApi.initLoadingStatus
            , groupId = Nothing
            , foundPeople = initPersons
            , waitingCheckIn = initPersons
            , searchLastName = ""
            , eventInfo = { eventName = "", eventId = "" }
            , personNotFound = Nothing
            , newPerson = initPerson
            , newPersons = initPersons
            , aggreedToSafetyWaiver = False
            , currentRoute = Router.Home
            , navbarState = navstate
            , popoverState = Popover.initialState
            , currentInfoEdit = 0
            }
    in
    ( mdl
    , Cmd.batch
        [ eventMsg
        , navmsg
        ]
    )


mainUpdate msg mdl =
    withCurrentPage mdl pages <| update msg mdl


mainView mdl =
    withCurrentPage mdl pages <|
        \c ->
            Grid.containerFluid [ class "mb-3" ]
                [ Grid.row
                    [ Row.attrs [ Flex.col, style [ ( "min-height", "100vh" ) ] ] ]
                    [ Grid.col [ Col.xsAuto ] [ navbar c pages mdl ]
                    , Grid.col [] [ view mdl c ]
                    ]
                ]
