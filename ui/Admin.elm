module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import Data as Data
import ErrorMsg as Err
import Html as Html exposing (Html, program, text)
import Html.Attributes exposing (attribute, class)


type alias Model =
    Data.HasCheckInGroup
        (Err.HasErrors
            { searchGroupId : String
            }
        )


type Msg
    = UpdateGroupId String
    | SearchGroupResponse (BreezeApi.Response Data.CheckInGroup)
    | SearchGroupClick


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
    , checkInGroup = Nothing
    , errors = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        UpdateGroupId s ->
            ( { mdl | searchGroupId = s }, Cmd.none )

        SearchGroupClick ->
            updateSearchGroupClick mdl

        SearchGroupResponse r ->
            updateCheckInGroup r mdl


updateSearchGroupClick : Model -> ( Model, Cmd Msg )
updateSearchGroupClick mdl =
    let
        m =
            String.toInt mdl.searchGroupId
                |> BreezeApi.fromResult (always 0) identity
                |> flip BreezeApi.getCheckInGroup SearchGroupResponse
    in
    ( mdl, m )


updateCheckInGroup : BreezeApi.Response Data.CheckInGroup -> Model -> ( Model, Cmd Msg )
updateCheckInGroup r mdl =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (flip Err.newError mdl)
                    (\p -> { mdl | checkInGroup = Just p })
    in
    ( m, Cmd.none )


view : Model -> Html Msg
view mdl =
    let
        checkInGroupRow =
            case mdl.checkInGroup of
                Nothing ->
                    []

                Just m ->
                    [ checkInGroupView m ]

        groupInputRow =
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs12 ] [ groupInputView mdl ] ]

        body =
            []
                |> List.append checkInGroupRow
                |> List.append [ groupInputRow ]
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.containerFluid [ class "mb-5" ] body
        ]


checkInGroupView : Data.CheckInGroup -> Html Msg
checkInGroupView mdl =
    Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xs12 ] [ groupPhotoView mdl.allowedPhotos ]
        , Grid.col [ Col.xs12 ] [ Data.listPersonView Nothing mdl.checkedInPersons ]
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


groupPhotoView : Bool -> Html Msg
groupPhotoView hasPhotos =
    let
        overlay =
            if hasPhotos then
                class "fas fa-check"
            else
                class "fas fa-times"
    in
    Html.i [ attribute "data-fa-mask" "fas fa-camera fa-3x", overlay ] []
