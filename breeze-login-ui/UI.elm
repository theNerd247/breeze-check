module Main exposing (..)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Html exposing (Html, text, program, div, button)
import Html.Attributes exposing (for, class)
import Html.Events exposing (onClick)
import Http as Http
import Json.Decode as Decode


type alias Person =
    { firstName : String
    , lastName : String
    , id : String
    }


type alias ErrorMessage =
    { errorMsg : String
    , errorId : Int
    }


type alias Model =
    { searchLastName : String
    , foundPeople : List Person
    , errs : List ErrorMessage
    }


type Msg
    = LastNameSearch
    | UpdateLastName String
    | CloseErrorMessage Int
    | FoundPeople (Result Http.Error (List Person))


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { foundPeople = []
    , errs = []
    , searchLastName = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        UpdateLastName s ->
            ( { mdl | searchLastName = s }, Cmd.none )

        LastNameSearch ->
            ( mdl, findPeople mdl.searchLastName )

        FoundPeople (Ok []) ->
            ( { mdl | errs = newErrorMessage ("I'm sorry nobody exists with the last name of: " ++ mdl.searchLastName) mdl.errs }, Cmd.none )

        FoundPeople (Ok ps) ->
            ( { mdl | foundPeople = ps }, Cmd.none )

        FoundPeople (Err e) ->
            ( { mdl | errs = newErrorMessage (toString e) mdl.errs }, Cmd.none )

        CloseErrorMessage eid ->
            ( { mdl
                | errs = List.filter (\x -> x.errorId /= eid) mdl.errs
              }
            , Cmd.none
            )


newErrorMessage : String -> List ErrorMessage -> List ErrorMessage
newErrorMessage msg msgs =
    let
        newId =
            Maybe.withDefault 0 << Maybe.map (\x -> 1 + x.errorId) <| List.head msgs

        newMessage =
            { errorId = newId, errorMsg = msg }
    in
        newMessage :: msgs


view : Model -> Html Msg
view mdl =
    Grid.containerFluid []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [] [ errors mdl.errs ] ]
        , Grid.row []
            [ Grid.col [] [ personSearch ]
            ]
        , Grid.row []
            [ Grid.col [] [ listPeople mdl.foundPeople ]
            ]
        ]


errors : List ErrorMessage -> Html Msg
errors =
    List.map error >> div []


error : ErrorMessage -> Html Msg
error msg =
    Alert.danger
        [ text msg.errorMsg
        , button [ class "close", onClick (CloseErrorMessage msg.errorId) ] [ text "x" ]
        ]


personSearch : Html Msg
personSearch =
    Form.form []
        [ Form.group []
            [ Form.label [ for "lastname" ] [ text "Last Name" ]
            , Input.text [ Input.id "lastname", Input.onInput UpdateLastName ]
            , Form.help [] [ text "Your family's last name" ]
            ]
        , Button.button [ Button.primary, Button.onClick LastNameSearch ] [ text "Find" ]
        ]


findPeople : String -> Cmd Msg
findPeople lastName =
    let
        getPeople =
            Http.get ("https://jsonplaceholder.typicode.com/users?username=" ++ lastName) decodePersons
    in
        Http.send FoundPeople getPeople


decodePersons : Decode.Decoder (List Person)
decodePersons =
    Decode.list <|
        Decode.map3
            Person
            (Decode.field "username" Decode.string)
            (Decode.field "email" Decode.string)
            (Decode.map toString (Decode.field "id" Decode.int))


listPeople : List Person -> Html Msg
listPeople =
    List.map (person >> List.singleton >> ListGroup.li []) >> ListGroup.ul


person : Person -> Html Msg
person p =
    Grid.row []
        [ Grid.col [] [ text p.id ]
        , Grid.col [] [ text p.firstName ]
        , Grid.col [] [ text p.lastName ]
        ]
