module UI exposing (..)

import Bootstrap.Navbar as Navbar
import Bootstrap.Popover as Popover
import BreezeApi as BreezeApi
import EventInfo as EventInfo
import Html exposing (body)
import Html.Attributes exposing (class)
import Nested exposing (modifyCmd)
import Pages exposing (..)
import Pages.CartPage as CartPage
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
    ]


main =
    mainWithRouter
        { init = init
        , update = mainUpdate
        , view = mainView
        , subscriptions = navbarSubscriptions
        }
        RouterMsg


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
            body
                [ class "mb-3" ]
                [ navbar c pages mdl
                , view mdl c
                ]
