module Admin exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import Data as Data
import ErrorMsg as Err
import Html as Html exposing (Html, h1, program, text)
import Html.Attributes exposing (attribute, class)
import Nested exposing (modifyBoth)
import UI as UI


type alias Model =
    BreezeApi.HasBreezeApi
        { searchGroupId : String
        , checkInGroup : List Data.Person
        , groupCheckedIn : Bool
        , ui : UI.Model
        , groupId : String
        }


type Msg
    = UpdateGroupId String
    | SearchGroupResponse (BreezeApi.Msg (List Data.Person))
    | SearchGroupClick
    | CheckInApprovedClick
    | CheckInApprovedResponse (BreezeApi.Msg Bool)
    | Err Err.Msg
    | UI UI.Msg


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


model : Model
model =
    { searchGroupId = ""
    , checkInGroup = []
    , errors = []
    , loadingStatus = False
    , groupCheckedIn = False
    , ui = UI.model
    , groupId = ""
    }


toGroupId : String -> Data.GroupId
toGroupId =
    String.toInt
        >> BreezeApi.fromResult (always 0) identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        UpdateGroupId s ->
            ( { mdl | searchGroupId = s }, Cmd.none )

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
            UI.update msg mdl.ui
                |> modifyBoth (\m -> { mdl | ui = m }) UI


updateSearchGroupClick : Model -> ( Model, Cmd Msg )
updateSearchGroupClick mdl =
    BreezeApi.getCheckInGroup SearchGroupResponse (toGroupId mdl.searchGroupId) { mdl | groupCheckedIn = False, checkInGroup = [] }


updateCheckInGroup : List Data.Person -> Model -> ( Model, Cmd Msg )
updateCheckInGroup ps mdl =
    ( { mdl | checkInGroup = ps, groupId = mdl.searchGroupId }, Cmd.none )


updateCheckInApprovedClicked : Model -> ( Model, Cmd Msg )
updateCheckInApprovedClicked mdl =
    BreezeApi.approveCheckIn CheckInApprovedResponse (toGroupId mdl.groupId) mdl


updateCheckInApprovedResponse : Bool -> Model -> ( Model, Cmd Msg )
updateCheckInApprovedResponse p mdl =
    ( { mdl | groupCheckedIn = p }, Cmd.none )


view : Model -> Html Msg
view mdl =
    let
        checkInGroupRow =
            [ checkInGroupView mdl ]

        groupInputRow =
            Grid.row
                [ Row.centerXs ]
                [ Grid.col [ Col.xs12 ] [ groupInputView mdl ] ]

        errors =
            [ Html.map Err <| Err.view mdl
            ]

        uiView =
            Html.map UI <| UI.view mdl.ui

        body =
            []
                |> List.append checkInGroupRow
                |> List.append [ groupInputRow ]
                |> List.append errors
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.row []
            [ Grid.col [ Col.xs12, Col.md6 ] body
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
            , Grid.col [ Col.xs12 ] [ Data.listPersonView Nothing mdl.checkInGroup ]
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
        [ Input.number
            [ Input.onInput UpdateGroupId
            , Input.value mdl.searchGroupId
            , Input.placeholder "Group Number"
            ]
        , Button.button [ Button.onClick SearchGroupClick ] [ text "Find" ]
        ]


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
