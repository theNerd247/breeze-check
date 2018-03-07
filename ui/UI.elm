module UI exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
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
import Navigation as Nav
import Nested exposing (modifyCmd)
import NewPerson as NewPerson
import RouteUrl as Url


type Page
    = Search
    | Select
    | NewPersons
    | Finished
    | Photo


type alias Model =
    NewPerson.HasNewFamilies
        (Find.HasFind
            (Err.HasErrors
                { page : Page
                , eventName : String
                }
            )
        )


type Msg
    = Find Find.Msg
    | Err Err.Msg
    | NewPerson NewPerson.Msg
    | SetPage Page
    | GetEventName
    | EventNameResult (BreezeApi.Msg Data.EventName)


main : Program Never Model Msg
main =
    program
        { init = getEventName model
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


app : Url.App Model Msg
app =
    { init = getEventName model
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    , delta2url = delta2url
    , location2messages = location2messages
    }


model : Model
model =
    { errors = []
    , loadingStatus = False
    , groupId = Nothing
    , foundPeople = []
    , waitingCheckIn = []
    , searchLastName = ""
    , page = Search
    , eventName = ""
    , personNotFound = False
    , newFamilies = NewPerson.initModel
    }



-- UPDATE


delta2url : Model -> Model -> Maybe Url.UrlChange
delta2url old current =
    Nothing


location2messages : Nav.Location -> List Msg
location2messages l =
    []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        mdl =
            m
                |> pageUpdate msg
                |> setPageUpdate msg
    in
    case msg of
        Find msg ->
            modifyCmd Find <| Find.update msg mdl

        Err msg ->
            ( Err.update msg mdl, Cmd.none )

        GetEventName ->
            getEventName mdl

        EventNameResult r ->
            BreezeApi.update eventNameResult r mdl

        NewPerson msg ->
            let
                f m ps =
                    { m
                        | waitingCheckIn =
                            ps
                                |> List.map (\p -> { p | checkedIn = True })
                                |> List.append m.waitingCheckIn
                        , page = Select
                        , searchLastName = ""
                        , personNotFound = False
                    }
            in
            modifyCmd NewPerson <| NewPerson.update f msg mdl

        _ ->
            ( mdl, Cmd.none )


getEventName : Model -> ( Model, Cmd Msg )
getEventName mdl =
    BreezeApi.eventInfo EventNameResult mdl


eventNameResult : Data.EventName -> Model -> ( Model, Cmd Msg )
eventNameResult n mdl =
    ( { mdl | eventName = n }, Cmd.none )


pageUpdate : Msg -> Model -> Model
pageUpdate msg mdl =
    case mdl.page of
        Search ->
            searchPageUpdate msg mdl

        Select ->
            selectPageUpdate msg mdl

        Finished ->
            finishedPageUpdate msg mdl

        Photo ->
            photoPageUpdate msg mdl

        _ ->
            mdl


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
        Find (Find.CheckInResponse (BreezeApi.Recieved (Ok (Ok _)))) ->
            { mdl | page = Photo }

        _ ->
            mdl


finishedPageUpdate : Msg -> Model -> Model
finishedPageUpdate msg mdl =
    case msg of
        Find Find.CancelCheckInClick ->
            { mdl | page = Select }

        _ ->
            mdl


photoPageUpdate : Msg -> Model -> Model
photoPageUpdate msg mdl =
    case msg of
        Find Find.CheckInClick ->
            { mdl | page = Finished }

        _ ->
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

        --tabs =
        --if
        --mdl.page
        --== Search
        --&& List.isEmpty mdl.foundPeople
        --&& List.isEmpty mdl.waitingCheckIn
        --then
        --div [] []
        --else
        --Grid.row [ Row.attrs [ class "fixed-bottom" ] ] <|
        --if mdl.page == Finished then
        --[]
        --else
        --[ Grid.col [ Col.xs12 ]
        --[ pageTabs mdl
        --]
        --]
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

                Photo ->
                    photoView mdl

        body =
            page
                |> List.append errors
                |> List.append titleRow
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.containerFluid [ class "mb-5" ] body

        --, tabs
        ]



--pageTabs : Model -> Html Msg
--pageTabs mdl =
--let
--searchIcon =
--Html.span []
--[ Html.i [ class "fa fa-search" ] []
--, text " Search"
--]
--selectIcon =
--Html.span []
--[ Html.i [ class "fa fa-list-ul" ] []
--, text " Check-in"
--]
--navIndex =
--case mdl.page of
--Search ->
--0
--Select ->
--1
--_ ->
---1
--in
--Navs.view navIndex
--[ Navs.navItem searchIcon (SetPage Search)
--, Navs.navItem selectIcon (SetPage Select)
--]


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
                        [ Button.button
                            [ Button.success
                            , Button.block
                            , Button.onClick (SetPage Photo)
                            ]
                            [ text "Next"
                            ]
                        ]
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
                    [ Html.map Find <| Find.cancelCheckInButton
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


photoView : Model -> List (Html Msg)
photoView mdl =
    [ Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xs12 ]
            [ h1 [] [ text "May We Take You're Picture Please?" ]
            , p []
                [ text
                    """
              We have a photographer here taking photos for our website and
              other promotional materials here at Mountain View Church. You may
              be in a few of our photos and we would like your permission to
              publish any photo taken that has you or your family in it.
                """
                ]
            , p []
                [ Html.b [] [ text "We won't publish any names or contant information!" ]
                ]
            ]
        ]
    , Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xsAuto ]
            [ Html.map Find <| Find.checkInButton
            ]
        ]
    ]
