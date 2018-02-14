module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Data as Breeze
import ErrorMsg as Err
import Html as Html
    exposing
        ( Html
        , a
        , br
        , button
        , div
        , h1
        , h4
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class, for)
import Html.Events exposing (onClick)
import Http as Http
import Time exposing (..)


type alias Model =
    { errors : Err.Errors
    , groupId : Maybe Int
    }


type alias Config =
    { eventName : String
    , debug : Bool
    }


type Msg
    = 

main : Program Never Model Msg
main =
    program
        { init = init
        , update = update config
        , view = view config
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { foundPeople = []
    , checkedIn = []
    , searchLastName = ""
    , errors = Err.model
    , debounce = Debounce.init
    , findPeopleLoading = False
    , groupId = Nothing
    }


config : Config
config =
    { eventName = "Test Event"
    , apiBase = "" -- "http://10.0.0.100:8080"
    , debug = False
    }



view : Config -> Model -> Html Msg
view cfg mdl =
    let
        titleRow =
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ h1 [{- class "display-1" -}] [ text cfg.eventName ]
                    , p [] [ text "Mountain View Church" ]
                    ]
                ]
            ]

        errorRow =
            [ Grid.row [] [ Grid.col [] [ Html.map ErrorMessage <| Err.view mdl.errors ] ] ]

        pageRow =
            case mdl.groupId of
                Nothing ->
                    viewCheckin cfg mdl

                Just gid ->
                    viewCheckedIn cfg gid

        body =
            []
                |> flip List.append titleRow
                |> flip List.append errorRow
                |> flip List.append pageRow
    in
    Grid.container [] body
