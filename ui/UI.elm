module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import CheckIn as CheckIn
import Data as Breeze
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
import Pages as Pages
import Time exposing (..)


type alias Model =
    CheckIn.HasCheckin {}


type alias Config =
    { eventName : String
    , debug : Bool
    }


type Msg
    = Find Find.Msg
    | CheckIn CheckIn.Msg
    | Err Err.Msg


main : Program Never Model Msg
main =
    program
        { init = CheckIn.model {}
        , update = update config
        , view = view config
        , subscriptions = \_ -> Sub.none
        }


config : Config
config =
    { eventName = "Test Event"
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

        body =
            []
                |> flip List.append titleRow
    in
    Grid.container [] body
