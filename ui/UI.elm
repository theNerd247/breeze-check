module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
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
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class, for)
import Nested exposing (modifyCmd)


type Page
    = Search
    | Select
    | Finished


type alias Model =
    CheckIn.HasCheckin
        (Find.HasFind
            (Err.HasErrors
                { page : Page
                , eventName : String
                }
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
    | SearchPageClick
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
    , page = Search
    , eventName = ""
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        mdl =
            pageUpdate msg m
    in
    case msg of
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
            selectPageUpdate (Debug.log "msg" msg) (Debug.log "mdl" mdl)

        Finished ->
            finishedPageUpdate msg mdl


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
        CheckIn CheckIn.CheckInClick ->
            { mdl | page = Finished }

        SearchPageClick ->
            { mdl | page = Search }

        _ ->
            mdl


finishedPageUpdate : Msg -> Model -> Model
finishedPageUpdate msg mdl =
    case msg of
        CheckIn CheckIn.CancelCheckInClick ->
            { mdl | page = Select }

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

        page =
            case mdl.page of
                Search ->
                    [ Html.map Find <| Find.searchPersonsView mdl ]

                Select ->
                    let
                        title =
                            Grid.row [ Row.centerXs ]
                                [ Grid.col [ Col.xs10 ]
                                    [ h2 [ class "text-center" ] [ text "Select Your Family Members" ]
                                    ]
                                ]
                    in
                    [ title
                    , Html.map Find <| Find.foundPeopleView mdl
                    , hr [] []
                    , Html.map Find <| Find.waitingCheckInView mdl
                    , br [] []
                    , selectPageButtonsView mdl
                    , hr [] []
                    ]

                Finished ->
                    [ Html.map CheckIn <| CheckIn.checkedInView mdl.groupId
                    , Html.map CheckIn <| CheckIn.cancelCheckInView
                    ]

        body =
            []
                |> flip List.append titleRow
                --|> flip List.append [ pageProgressView mdl.page ]
                |> flip List.append errors
                |> flip List.append page
    in
    Grid.container [] body


pageProgressView : Page -> Html msg
pageProgressView pg =
    let
        ops =
            List.append [] <|
                -- Progress.height 10 ] <|
                case pg of
                    Search ->
                        [ Progress.value 33 ]

                    Select ->
                        [ Progress.value 66, Progress.info ]

                    Finished ->
                        [ Progress.value 100, Progress.success ]
    in
    Grid.row [] [ Grid.col [ Col.xs12 ] [ Progress.progress ops ] ]


selectPageButtonsView : Model -> Html Msg
selectPageButtonsView mdl =
    Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xsAuto ] <|
            [ searchPageButtonView
            ]
        , Grid.col [ Col.xsAuto ] <|
            if not <| List.isEmpty mdl.waitingCheckIn then
                [ Html.map CheckIn <| CheckIn.checkInButtonView ]
            else
                []
        ]


searchPageButtonView : Html Msg
searchPageButtonView =
    Button.button
        [ Button.large
        , Button.onClick SearchPageClick
        , Button.outlineSecondary
        ]
        [ Html.i [ class "fas fa-arrow-left" ] []
        , text " Search"
        ]
