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
    Err.HasErrors
        { searchGroupId : String
        , checkInGroup : List Data.Person
        , groupCheckedIn : Bool
        , ui : UI.Model
        }


type Msg
    = UpdateGroupId String
    | SearchGroupResponse (BreezeApi.Response (List Data.Person))
    | SearchGroupClick
    | CheckInApprovedClick
    | CheckInApprovedResponse (BreezeApi.Response Bool)
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
    , groupCheckedIn = False
    , ui = UI.model
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
            updateCheckInGroup r mdl

        CheckInApprovedClick ->
            updateCheckInApprovedClicked mdl

        CheckInApprovedResponse r ->
            updateCheckInApprovedResponse r mdl

        Err msg ->
            ( Err.update msg mdl, Cmd.none )

        UI msg ->
            UI.update msg mdl.ui
                |> modifyBoth (\m -> { mdl | ui = m }) UI


updateSearchGroupClick : Model -> ( Model, Cmd Msg )
updateSearchGroupClick mdl =
    ( { mdl | groupCheckedIn = False, checkInGroup = [] }, BreezeApi.getCheckInGroup (toGroupId mdl.searchGroupId) SearchGroupResponse )


updateCheckInGroup : BreezeApi.Response (List Data.Person) -> Model -> ( Model, Cmd Msg )
updateCheckInGroup r mdl =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (flip Err.newError mdl)
                    (\p -> { mdl | checkInGroup = p })
    in
    ( m, Cmd.none )


updateCheckInApprovedClicked : Model -> ( Model, Cmd Msg )
updateCheckInApprovedClicked mdl =
    ( mdl, BreezeApi.approveCheckIn (toGroupId mdl.searchGroupId) CheckInApprovedResponse )


updateCheckInApprovedResponse : BreezeApi.Response Bool -> Model -> ( Model, Cmd Msg )
updateCheckInApprovedResponse r mdl =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (flip Err.newError mdl)
                    (\p -> { mdl | groupCheckedIn = p })
    in
    ( m, Cmd.none )


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
            [ Grid.col [] body
            , Grid.col [] [ uiView ]
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
