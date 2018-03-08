module UI exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import BreezeApi as BreezeApi
import ErrorMsg as Err
import EventName as Event
import FindPeople as Find
import Html as Html
    exposing
        ( Html
        , div
        , h1
        , h2
        , h3
        , h4
        , p
        , text
        )
import Html.Attributes exposing (class, for, style)
import Nested exposing (modifyCmd)
import NewPerson as NewPerson
import Pages.PhotoPage as PhotoPage exposing (HasPhotoPage)
import Pages.SearchPage as SearchPage exposing (HasSearchPage)
import Pages.SelectPage as SelectPage exposing (HasSelectPage)
import Pages.WaitingApprovalPage as WaitingApprovalPage exposing (HasWaitingApprovalPage)
import Router as Router exposing (HasRoutes, mainWithRouter)


type alias Model =
    HasRoutes (BreezeApi.HasBreezeApi (Event.HasEventName (Find.HasFind (NewPerson.HasNewFamilies {}))))


type Msg
    = RouterMsg Router.Msg
    | Error Err.Msg
    | EventName Event.Msg
    | SearchPage SearchPage.Msg
    | SelectPage SelectPage.Msg
    | PhotoPage PhotoPage.Msg
    | WaitingApprovalPage WaitingApprovalPage.Msg


main =
    --Html.program
    mainWithRouter
        { init = initPages model
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
        RouterMsg


model : Model
model =
    { errors = []
    , loadingStatus = BreezeApi.initLoadingStatus
    , groupId = Nothing
    , foundPeople = []
    , waitingCheckIn = []
    , searchLastName = ""
    , eventName = ""
    , personNotFound = False
    , newFamilies = NewPerson.initModel
    , currentRoute = Router.Search
    }


initPages : Model -> ( Model, Cmd Msg )
initPages mdl =
    ( mdl, Cmd.map (always (EventName Event.GetEventName)) Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        RouterMsg msg ->
            modifyCmd RouterMsg <| Router.update msg mdl

        Error msg ->
            ( Err.update msg mdl, Cmd.none )

        EventName msg ->
            modifyCmd EventName <| Event.update msg mdl

        SearchPage msg ->
            modifyCmd SearchPage <| SearchPage.update msg mdl

        SelectPage msg ->
            modifyCmd SelectPage <| SelectPage.update msg mdl

        PhotoPage msg ->
            modifyCmd PhotoPage <| PhotoPage.update msg mdl

        WaitingApprovalPage msg ->
            modifyCmd WaitingApprovalPage <| WaitingApprovalPage.update msg mdl


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
            [ Html.map Error <| Err.view mdl
            ]

        loading =
            if mdl.loadingStatus == BreezeApi.Waiting then
                [ Grid.row [ Row.centerXs ]
                    [ Grid.col [ Col.xs12 ]
                        [ loadingBar
                        ]
                    ]
                ]
            else
                []

        page =
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ viewPage mdl
                    ]
                ]
            ]

        body =
            errors
                |> List.append loading
                |> List.append titleRow
                |> flip List.append page
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.containerFluid [ class "mb-5" ] body
        ]


viewPage : Model -> Html Msg
viewPage mdl =
    case mdl.currentRoute of
        Router.Search ->
            Html.map SearchPage <| SearchPage.view mdl

        Router.Selected ->
            Html.map SelectPage <| SelectPage.view mdl

        Router.Photo ->
            Html.map PhotoPage <| PhotoPage.view mdl

        Router.WaitingApproval ->
            Html.map WaitingApprovalPage <| WaitingApprovalPage.view mdl


loadingBar : Html msg
loadingBar =
    Progress.progress
        [ Progress.value 100
        , Progress.animated
        ]
