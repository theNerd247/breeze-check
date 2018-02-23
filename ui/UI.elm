module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import CheckIn as CheckIn
import Data as Data
import ErrorMsg as Err
import FindPeople as Find
import Html as Html
    exposing
        ( Html
        , a
        , br
        , button
        , div
        , h1
        , h2
        , h3
        , h4
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class, for, style)
import Navs as Navs
import Nested exposing (modifyCmd)
import NewPerson as NewPerson


type Page
    = Search
    | Select
    | NewPersons
    | Finished


type alias Model =
    NewPerson.HasNewPersons
        (CheckIn.HasCheckin
            (Find.HasFind
                (Err.HasErrors
                    { page : Page
                    , eventName : String
                    }
                )
            )
        )


type alias Config =
    { eventName : String
    , debug : Bool
    }


type Msg
    = Find Find.Msg
    | CheckIn CheckIn.Msg
    | Err Err.Msg
    | NewPerson NewPerson.Msg
    | SetPage Page
    | GetEventName
    | EventNameResult (BreezeApi.Response Data.EventName)


main : Program Never Model Msg
main =
    program
        { init = getEventName model
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


model : Model
model =
    { errors = []
    , groupId = Nothing
    , foundPeople = []
    , waitingCheckIn = []
    , searchLastName = ""
    , findPeopleLoading = False
    , page = NewPersons
    , eventName = ""
    , personNotFound = False
    , newPersons = NewPerson.initModel
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        mdl =
            m
                |> pageUpdate msg
                |> setPageUpdate msg
    in
    case Debug.log "msg: " msg of
        Find msg ->
            modifyCmd Find <| Find.update msg mdl

        CheckIn msg ->
            modifyCmd CheckIn <| CheckIn.update msg mdl

        Err msg ->
            ( Err.update msg mdl, Cmd.none )

        GetEventName ->
            getEventName mdl

        EventNameResult r ->
            eventNameResult mdl r

        NewPerson msg ->
            modifyCmd NewPerson <| NewPerson.update msg mdl

        _ ->
            ( mdl, Cmd.none )


getEventName : Model -> ( Model, Cmd Msg )
getEventName mdl =
    ( mdl, BreezeApi.eventInfo EventNameResult )


eventNameResult : Model -> BreezeApi.Response Data.EventName -> ( Model, Cmd Msg )
eventNameResult mdl r =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (\e -> Err.newError e mdl)
                    (\n -> { mdl | eventName = n })
    in
    ( m, Cmd.none )


pageUpdate : Msg -> Model -> Model
pageUpdate msg mdl =
    case mdl.page of
        Search ->
            searchPageUpdate msg mdl

        Select ->
            selectPageUpdate msg mdl

        Finished ->
            finishedPageUpdate msg mdl

        NewPersons ->
            newPersonsUpdate msg mdl


setPageUpdate : Msg -> Model -> Model
setPageUpdate msg mdl =
    case msg of
        SetPage p ->
            { mdl | page = p }

        _ ->
            mdl



-- UPDATE functions to occur only when on specific pages


searchPageUpdate : Msg -> Model -> Model
searchPageUpdate msg mdl =
    case msg of
        Find Find.SearchClick ->
            { mdl | page = Select }

        _ ->
            mdl


selectPageUpdate : Msg -> Model -> Model
selectPageUpdate msg mdl =
    case msg of
        CheckIn (CheckIn.CheckInResponse (Ok (Ok _))) ->
            { mdl | page = Finished }

        _ ->
            mdl


finishedPageUpdate : Msg -> Model -> Model
finishedPageUpdate msg mdl =
    case msg of
        CheckIn CheckIn.CancelCheckInClick ->
            { mdl | page = Select }

        _ ->
            mdl


newPersonsUpdate : Msg -> Model -> Model
newPersonsUpdate _ mdl =
    mdl



-- VIEW


view : Model -> Html Msg
view mdl =
    let
        titleRow =
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ h1 [] [ text mdl.eventName ]
                    , p [] [ text "Mountain View Church" ]
                    ]
                ]
            ]

        errors =
            [ Html.map Err <| Err.view mdl
            ]

        tabs =
            if
                mdl.page
                    == Search
                    && List.isEmpty mdl.foundPeople
                    && List.isEmpty mdl.waitingCheckIn
            then
                div [] []
            else
                Grid.row [ Row.attrs [ class "fixed-bottom" ] ] <|
                    if mdl.page == Finished then
                        []
                    else
                        [ Grid.col [ Col.xs12 ]
                            [ pageTabs mdl
                            ]
                        ]

        page =
            case mdl.page of
                Search ->
                    searchPageView mdl

                Select ->
                    selectPageView mdl

                Finished ->
                    finishedPageView mdl

                NewPersons ->
                    newPersonsView mdl

        body =
            page
                |> List.append errors
                |> List.append titleRow
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.containerFluid [ class "mb-5" ] body
        , tabs
        ]


pageTabs : Model -> Html Msg
pageTabs mdl =
    let
        searchIcon =
            Html.span []
                [ Html.i [ class "fa fa-search" ] []
                , text " Search"
                ]

        selectIcon =
            Html.span []
                [ Html.i [ class "fa fa-list-ul" ] []
                , text " Check-in"
                ]

        navIndex =
            case mdl.page of
                Search ->
                    0

                Select ->
                    1

                _ ->
                    -1
    in
    Navs.view navIndex
        [ Navs.navItem searchIcon (SetPage Search)
        , Navs.navItem selectIcon (SetPage Select)
        ]


searchPageView : Model -> List (Html Msg)
searchPageView mdl =
    let
        title =
            Grid.row []
                [ Grid.col [ Col.xsAuto ]
                    [ h2 [] [ text "Find Your Family" ]
                    ]
                ]

        searchForm =
            Grid.row []
                [ Grid.col [ Col.xs12 ]
                    [ Html.map Find <| Find.searchPersonsView mdl
                    ]
                ]
    in
    [ title
    , searchForm
    ]


selectPageView : Model -> List (Html Msg)
selectPageView mdl =
    let
        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs12 ]
                    [ h4 [ class "text-center" ] [ text "Select Your Family Members" ]
                    ]
                ]

        found =
            Grid.row [ Row.attrs [ class "pb-3" ], Row.centerXs ]
                [ Grid.col [ Col.xs12 ] <|
                    [ Html.map Find <| Find.foundPeopleView mdl
                    ]
                ]

        waiting =
            let
                a =
                    if not <| List.isEmpty mdl.waitingCheckIn then
                        [ class "pb-3" ]
                    else
                        []
            in
            Grid.row [ Row.attrs a, Row.centerXs ]
                [ Grid.col [ Col.xs12 ]
                    [ Html.map Find <| Find.waitingCheckInView mdl
                    ]
                ]

        backButton =
            if mdl.personNotFound then
                Grid.row [ Row.attrs [ class "pb-3" ] ]
                    [ Grid.col [ Col.xs12 ]
                        [ Button.button
                            [ Button.onClick (SetPage Search)
                            , Button.primary
                            , Button.block
                            ]
                            [ Html.i [ class "fas fa-search" ] []
                            , text <| " Search"
                            ]
                        ]
                    ]
            else
                div [] []

        checkInButton =
            Grid.row [ Row.attrs [ class "pb-3" ] ] <|
                if not <| List.isEmpty mdl.waitingCheckIn then
                    [ Grid.col [ Col.xs12 ]
                        [ Html.map CheckIn <| CheckIn.checkInButtonView ]
                    ]
                else
                    []

        addFamilyButtons =
            let
                addFamily =
                    Grid.col [ Col.xs12, Col.attrs [ class "pb-3" ] ]
                        [ Button.button
                            [ Button.onClick (SetPage Search)
                            , Button.secondary
                            , Button.block
                            ]
                            [ text "Add Another Family To Check In"
                            ]
                        ]

                newFamily =
                    Grid.col [ Col.xs12, Col.attrs [ class "pb-3" ] ]
                        [ Button.button
                            [ Button.info
                            , Button.block
                            , Button.onClick (SetPage NewPersons)
                            ]
                            [ text "I can't find us!"
                            ]
                        ]
            in
            Grid.row [ Row.centerXs ] <|
                if not <| List.isEmpty mdl.waitingCheckIn then
                    [ addFamily
                    , newFamily
                    ]
                else
                    [ newFamily ]

        header =
            Grid.row [ Row.centerXs ] <|
                if not <| List.isEmpty mdl.waitingCheckIn then
                    [ Grid.col [ Col.xsAuto ]
                        [ h3 [] [ text "You're checking in" ]
                        ]
                    ]
                else
                    []
    in
    [ title
    , found
    , backButton
    , header
    , waiting
    , addFamilyButtons
    , checkInButton
    ]


finishedPageView : Model -> List (Html Msg)
finishedPageView mdl =
    let
        title =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h2 [] [ text "You're Almost Done!" ]
                    ]
                ]

        inst =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs10 ]
                    [ p [ class "text-center" ]
                        [ text "Stop by the check-in desk and show them this number"
                        ]
                    ]
                ]

        cancelCheckin =
            Grid.row [ Row.centerXs, Row.attrs [ class "pb-3" ] ]
                [ Grid.col [ Col.xsAuto ]
                    [ Html.map CheckIn <| CheckIn.cancelCheckInView
                    ]
                ]

        groupId gid =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h1 [] [ text <| toString gid ]
                    ]
                ]

        checkInTitle =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs10 ]
                    [ h3 [] [ text "You're Checking In" ]
                    ]
                ]
    in
    case mdl.groupId of
        Nothing ->
            []

        Just gid ->
            [ title
            , inst
            , groupId gid
            , cancelCheckin
            , checkInTitle
            , Data.listPersonView Nothing mdl.waitingCheckIn
            ]


newPersonsView : Model -> List (Html Msg)
newPersonsView mdl =
    [ Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xs12 ]
            [ Html.map NewPerson <| NewPerson.newFamilysView mdl
            ]
        ]
    ]
