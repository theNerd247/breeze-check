module UI exposing (..)

import Pages exposing (..)
import Pages.CartPage as CartPage
import Pages.HomePage as HomePage
import Pages.PhotoPage as PhotoPage
import Pages.SearchPage as SearchPage
import Pages.SelectPage as SelectPage
import Pages.WaitingApprovalPage as WaitingApprovalPage
import Router as Router exposing (mainWithRouter)


pages : Pages.Config
pages =
    [ HomePage.config
    , SearchPage.config
    , SelectPage.config
    , NewPersonsPage.config
    , CartPage.config
    , PhotoPage.config
    , WaitingApprovalPage.config
    ]


main =
    mainWithRouter
        { init = init
        , update = mainUpdate
        , view = mainView
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
            , eventInfo = { eventName = "", eventId = "" }
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


mainUpdate : Msg -> Model -> ( Model, Cmd Msg )
mainUpdate msg mdl =
    pages
        |> withCurrentPage mdl update


mainView : Model -> Html Msg
mainView mdl =
    pages
        |> withCurrentPage mdl view
