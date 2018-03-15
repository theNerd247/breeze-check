module Admin exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import BreezeApi as BreezeApi
import Data as Data
import Dict as Dict
import ErrorMsg as Err
import Html as Html exposing (Html, br, div, h1, h4, hr, p, program, text)
import Html.Attributes exposing (attribute, class, style)
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
        , groupNotFound : Bool
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
            , groupNotFound = False
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
    BreezeApi.getCheckInGroup SearchGroupResponse
        (toCheckInGroupId
            mdl.searchCheckInGroupId
        )
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
            ( { mdl
                | checkInGroup = ps
                , groupId = mdl.searchCheckInGroupId
              }
            , Cmd.none
            )


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
                [ Grid.col [ Col.xs12 ] [ groupInputView mdl ] ]

        errors =
            Html.map Err <| Err.view mdl

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
                    |> Person.extraCol (groupPhotoView << .wantsPhotos)
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
                , Input.value mdl.searchCheckInGroupId
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
                Html.i [ class "fas fa-times text-success" ] []
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
