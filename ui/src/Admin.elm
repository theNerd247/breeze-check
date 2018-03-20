module Admin exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Display as Display
import BreezeApi as BreezeApi
import Data as Data
import Dict as Dict
import ErrorMsg as Err
import EventInfo as EventInfo
import FindPeople as Find
import Html as Html exposing (Html, br, div, h1, h4, hr, p, program, text)
import Html.Attributes exposing (attribute, class, style, value)
import Nested exposing (modifyBoth, modifyMdl)
import Pages as Pages
import Person as Person
import Result as Result
import Router as Router
import Tuple as Tuple
import UI as UI


type alias Model =
    BreezeApi.HasBreezeApi
        { searchCheckInGroupId : Maybe Data.CheckInGroupId
        , checkInGroup : List Data.Person
        , groupCheckedIn : Bool
        , ui : Pages.Model
        , groupId : Maybe Data.CheckInGroupId
        , groupNotFound : Bool
        , eventInfoList : List Data.EventInfo
        }


type Msg
    = UpdateCheckInGroupId String
    | SearchGroupResponse (BreezeApi.Msg (List Data.Person))
    | SearchGroupClick
    | CheckInApprovedClick
    | CheckInApprovedResponse (BreezeApi.Msg Bool)
    | Err Err.Msg
    | UI Pages.Msg
    | GetEventInfoListReturn (BreezeApi.Msg (List Data.EventInfo))
    | SetEventInfo Data.EventId


uiProg =
    UI.uiProg


main : Program Never Model Msg
main =
    let
        model =
            { searchCheckInGroupId = Nothing
            , checkInGroup = []
            , errors = []
            , loadingStatus = BreezeApi.initLoadingStatus
            , groupCheckedIn = False
            , ui = Tuple.first uiProg.init
            , groupId = Nothing
            , groupNotFound = False
            , eventInfoList = []
            }

        --( mdl, emsg ) =
        --BreezeApi.getEventList GetEventInfoListReturn model
        --TOOD: uncomment after easter egg hunt
    in
    program
        { init =
            ( model
            , Cmd.batch
                [ Cmd.map UI <| Tuple.second uiProg.init

                --, emsg --TODO: uncomment after easter egg hunt
                ]
            )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        UpdateCheckInGroupId s ->
            ( { mdl | searchCheckInGroupId = s |> String.toInt |> Result.toMaybe }, Cmd.none )

        SearchGroupClick ->
            updateSearchGroupClick mdl

        SearchGroupResponse r ->
            BreezeApi.update updateCheckInGroup r mdl

        CheckInApprovedClick ->
            updateCheckInApprovedClicked mdl

        CheckInApprovedResponse r ->
            BreezeApi.update updateCheckInApprovedResponse r mdl

        Err msg ->
            ( Err.update msg mdl, Cmd.none )

        UI msg ->
            let
                ( newUI, uiCmd ) =
                    UI.mainUpdate msg mdl.ui
                        |> modifyBoth (\m -> { mdl | ui = m }) UI
            in
            if mdl.ui.groupId == Nothing && newUI.ui.groupId /= Nothing then
                let
                    ( m, cm ) =
                        updateSearchGroupClick { newUI | searchCheckInGroupId = newUI.ui.groupId }
                in
                ( m, Cmd.batch [ uiCmd, cm ] )
            else
                ( newUI, uiCmd )

        GetEventInfoListReturn r ->
            BreezeApi.update getEventListResult r mdl

        SetEventInfo eid ->
            let
                ui =
                    mdl.ui
            in
            modifyMdl (\m -> { mdl | ui = m }) <| BreezeApi.setEventInfo eid (UI << Pages.EventInfoMsg << EventInfo.EventInfoResult) mdl.ui


getEventListResult : List Data.EventInfo -> Model -> ( Model, Cmd Msg )
getEventListResult es mdl =
    ( { mdl | eventInfoList = es }, Cmd.none )


updateSearchGroupClick : Model -> ( Model, Cmd Msg )
updateSearchGroupClick mdl =
    case mdl.searchCheckInGroupId of
        Nothing ->
            ( mdl, Cmd.none )

        Just gid ->
            BreezeApi.getCheckInGroup SearchGroupResponse
                gid
                { mdl
                    | groupCheckedIn = False
                    , checkInGroup = []
                    , groupNotFound = False
                }


updateCheckInGroup : List Data.Person -> Model -> ( Model, Cmd Msg )
updateCheckInGroup ps mdl =
    case ps of
        [] ->
            ( { mdl
                | groupNotFound = True
                , groupId = mdl.searchCheckInGroupId
              }
            , Cmd.none
            )

        _ ->
            let
                ui =
                    mdl.ui

                newUI =
                    { ui
                        | currentRoute = Router.WaitingApproval
                        , waitingCheckIn =
                            ps
                                |> List.map (\p -> ( p.pid, p ))
                                |> Dict.fromList
                        , groupId = mdl.searchCheckInGroupId
                    }
            in
            ( { mdl
                | checkInGroup = ps
                , ui = newUI
                , groupId = mdl.searchCheckInGroupId
              }
            , Cmd.none
            )


updateCheckInApprovedClicked : Model -> ( Model, Cmd Msg )
updateCheckInApprovedClicked mdl =
    case mdl.groupId of
        Nothing ->
            ( mdl, Cmd.none )

        Just gid ->
            BreezeApi.approveCheckIn CheckInApprovedResponse gid mdl


updateCheckInApprovedResponse : Bool -> Model -> ( Model, Cmd Msg )
updateCheckInApprovedResponse p mdl =
    ( { mdl | groupCheckedIn = p }, Cmd.none )


view : Model -> Html Msg
view mdl =
    let
        checkInGroupRow =
            checkInGroupView mdl

        groupInputRow =
            Grid.row
                [ Row.centerXs ]
                [ Grid.col [ Col.xs12 ] [ groupInputView mdl ] ]

        errors =
            Html.map Err <| Err.view mdl

        eventInfoRow =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ eventInfoListView
                        mdl.eventInfoList
                    ]
                ]

        uiView =
            Html.map UI <| UI.mainView mdl.ui

        title =
            h1 [] [ text "Admin" ]

        body =
            div [ class "row flex-column h-100 mx-1" ]
                [ div [ class "text-center", style [ ( "flex-grow", "1" ) ] ] [ title ]
                , div [ style [ ( "flex-grow", "12" ) ] ]
                    --, Grid.col [ Col.xs10, Col.attrs [ Size.w100 ] ]
                    [ errors
                    , br [] []
                    , groupInputRow
                    , br [] []
                    , checkInGroupRow

                    --, eventInfoRow --TODO: uncomment when easter egg hunt is
                    --finished
                    ]
                ]
    in
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col [ Col.md6, Col.xs12 ] [ body ]
            , Grid.col [ Col.attrs [ Display.noneMd ] ] [ hr [] [] ]
            , Grid.col [ Col.md6, Col.xs12 ] [ uiView ]
            ]
        ]


checkInGroupView : Model -> Html Msg
checkInGroupView mdl =
    Grid.row [ Row.centerXs, Row.attrs [ class "text-center" ] ] <|
        if List.isEmpty mdl.checkInGroup && not mdl.groupNotFound then
            []
        else if List.isEmpty mdl.checkInGroup && mdl.groupNotFound then
            [ Grid.col [ Col.xs12 ]
                [ p [ class "text-danger" ]
                    [ text <|
                        "The group for "
                            ++ toString mdl.groupId
                            ++ " doesn't have any members"
                    ]
                ]
            ]
        else
            let
                ps =
                    mdl.checkInGroup
                        |> List.map (\p -> ( p.pid, p ))
                        |> Dict.fromList
            in
            [ Grid.col [ Col.xs12 ]
                [ Person.config
                    |> Person.cols
                        [ groupPhotoView << .wantsPhotos
                        , \p -> text p.personName.firstName
                        , \p -> text p.personName.lastName
                        ]
                    |> Person.view ps
                ]
            , Grid.col [ Col.xs12 ] <|
                if mdl.groupCheckedIn then
                    [ h1 []
                        [ checkMark
                        , text " Checked In!"
                        ]
                    ]
                else
                    [ approveCheckInButton ]
            ]


groupInputView : Model -> Html Msg
groupInputView mdl =
    Form.form []
        [ InputGroup.config
            (InputGroup.text
                [ Input.onInput UpdateCheckInGroupId
                , Input.value
                    (mdl.searchCheckInGroupId
                        |> Maybe.map toString
                        |> Maybe.withDefault ""
                    )
                , Input.placeholder "Group Number"
                ]
            )
            |> InputGroup.successors
                [ InputGroup.button [ Button.onClick SearchGroupClick ] [ text "Find" ]
                ]
            |> InputGroup.view
        ]


approveCheckInButton : Html Msg
approveCheckInButton =
    Button.button [ Button.onClick CheckInApprovedClick ] [ text "Approve Check-In" ]


groupPhotoView : Bool -> Html Msg
groupPhotoView hasPhotos =
    let
        status =
            if hasPhotos then
                Html.i [ class "fas fa-check text-success" ] []
            else
                Html.i [ class "fas fa-times text-danger" ] []
    in
    h4 []
        [ Html.i [ class "fas fa-camera" ] []
        , status
        ]


checkMark : Html msg
checkMark =
    Html.i [ class "fas fa-check text-success" ] []


eventInfoListView : List Data.EventInfo -> Html Msg
eventInfoListView es =
    Form.form []
        [ Select.custom
            [ Select.onChange <| SetEventInfo
            ]
          <|
            List.map
                (\e -> Select.item [ value e.eventId ] [ text e.eventName ])
                es
        ]
