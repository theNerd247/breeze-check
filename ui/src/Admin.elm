module Admin exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import Data as Data
import Dict as Dict
import ErrorMsg as Err
import Html as Html exposing (Html, h1, program, text)
import Html.Attributes exposing (attribute, class)
import Nested exposing (modifyBoth)
import Pages as Pages
import Person as Person
import Tuple as Tuple
import UI as UI


type alias Model =
    BreezeApi.HasBreezeApi
        { searchCheckInGroupId : String
        , checkInGroup : List Data.Person
        , groupCheckedIn : Bool
        , ui : Pages.Model
        , groupId : String
        }


type Msg
    = UpdateCheckInGroupId String
    | SearchGroupResponse (BreezeApi.Msg (List Data.Person))
    | SearchGroupClick
    | CheckInApprovedClick
    | CheckInApprovedResponse (BreezeApi.Msg Bool)
    | Err Err.Msg
    | UI Pages.Msg


uiProg =
    UI.uiProg


main : Program Never Model Msg
main =
    let
        model =
            { searchCheckInGroupId = ""
            , checkInGroup = []
            , errors = []
            , loadingStatus = BreezeApi.initLoadingStatus
            , groupCheckedIn = False
            , ui = Tuple.first uiProg.init
            , groupId = ""
            }
    in
    program
        { init = ( model, Cmd.map UI <| Tuple.second uiProg.init )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


toCheckInGroupId : String -> Data.CheckInGroupId
toCheckInGroupId =
    String.toInt
        >> BreezeApi.fromResult (always 0) identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        UpdateCheckInGroupId s ->
            ( { mdl | searchCheckInGroupId = s }, Cmd.none )

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
            UI.mainUpdate msg mdl.ui
                |> modifyBoth (\m -> { mdl | ui = m }) UI


updateSearchGroupClick : Model -> ( Model, Cmd Msg )
updateSearchGroupClick mdl =
    BreezeApi.getCheckInGroup SearchGroupResponse (toCheckInGroupId mdl.searchCheckInGroupId) { mdl | groupCheckedIn = False, checkInGroup = [] }


updateCheckInGroup : List Data.Person -> Model -> ( Model, Cmd Msg )
updateCheckInGroup ps mdl =
    ( { mdl | checkInGroup = ps, groupId = mdl.searchCheckInGroupId }, Cmd.none )


updateCheckInApprovedClicked : Model -> ( Model, Cmd Msg )
updateCheckInApprovedClicked mdl =
    BreezeApi.approveCheckIn CheckInApprovedResponse (toCheckInGroupId mdl.groupId) mdl


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
                [ Grid.col [ Col.xs6 ] [ groupInputView mdl ] ]

        errors =
            Html.map Err <| Err.view mdl

        uiView =
            Html.map UI <| UI.mainView mdl.ui

        body =
            Grid.row []
                [ Grid.col []
                    [ checkInGroupRow
                    , groupInputRow
                    , errors
                    ]
                ]
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.row []
            [ Grid.col [ Col.xs12, Col.md6, Col.middleXs ] [ body ]
            , Grid.col [ Col.xs12, Col.md6 ] [ uiView ]
            ]
        ]


checkInGroupView : Model -> Html Msg
checkInGroupView mdl =
    Grid.row [ Row.centerXs ] <|
        if List.isEmpty mdl.checkInGroup then
            []
        else
            [ Grid.col [ Col.xs12 ] [ groupPhotoView False ]
            , Grid.col [ Col.xs12 ]
                [ Person.onlyListPersons <|
                    Dict.fromList <|
                        List.map (\p -> ( p.pid, p )) mdl.checkInGroup
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
    InputGroup.config
        (InputGroup.text
            [ Input.onInput UpdateCheckInGroupId
            , Input.value mdl.searchCheckInGroupId
            , Input.placeholder "Group Number"
            ]
        )
        |> InputGroup.successors
            [ InputGroup.button [ Button.onClick SearchGroupClick ] [ text "Find" ]
            ]
        |> InputGroup.view


approveCheckInButton : Html Msg
approveCheckInButton =
    Button.button [ Button.onClick CheckInApprovedClick ] [ text "Approve Check-In" ]


groupPhotoView : Bool -> Html Msg
groupPhotoView hasPhotos =
    let
        status =
            if hasPhotos then
                checkMark
            else
                Html.i [ class "fas fa-times text-danger" ] []
    in
    h1 []
        [ Html.i [ class "fas fa-camera" ] []
        , status
        ]


checkMark : Html msg
checkMark =
    Html.i [ class "fas fa-check text-success" ] []
