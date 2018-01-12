module Main exposing (..)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Html
    exposing
        ( Html
        , text
        , program
        , div
        , button
        , h1
        , p
        , header
        , main_
        , hr
        , a
        )
import Html.Attributes exposing (for, class)
import Html.Events exposing (onClick)
import Http as Http
import Json.Decode as Decode


type alias PersonId =
    String


type alias Person =
    { firstName : String
    , lastName : String
    , id : PersonId
    , attending : Bool
    }


type alias ErrorMessage =
    { errorMsg : String
    , errorId : Int
    }


type alias Model =
    { searchLastName : String
    , foundPeople : List Person
    , checkedIn : List Person
    , errs : List ErrorMessage
    }


type alias Config =
    { eventName : String
    }


type Msg
    = LastNameSearch
    | UpdateLastName String
    | CloseErrorMessage Int
    | FoundPeople (Result Http.Error (List Person))
    | ToggleAttending PersonId


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
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
    , errs = []
    , searchLastName = ""
    }


config : Config
config =
    { eventName = "Test Event" }


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

        ToggleAttending pid ->
            let
                ( chin, fnd ) =
                    toggleCheckIn pid ( mdl.checkedIn, mdl.foundPeople )
            in
                ( { mdl | checkedIn = chin, foundPeople = fnd }, Cmd.none )


toggleCheckIn : PersonId -> ( List Person, List Person ) -> ( List Person, List Person )
toggleCheckIn pid ( chkin, found ) =
    let
        personFilter p =
            p.id == pid

        toggleAttend p =
            { p | attending = not p.attending }

        ( ci, co ) =
            List.partition personFilter chkin

        ( fi, fo ) =
            List.partition personFilter found

        newChkin =
            List.append (List.map toggleAttend fi) co

        newFound =
            List.append (List.map toggleAttend ci) fo
    in
        ( newChkin, newFound )


newErrorMessage : String -> List ErrorMessage -> List ErrorMessage
newErrorMessage msg msgs =
    let
        newId =
            Maybe.withDefault 0 << Maybe.map (\x -> 1 + x.errorId) <| List.head msgs

        newMessage =
            { errorId = newId, errorMsg = msg }
    in
        newMessage :: msgs


view : Config -> Model -> Html Msg
view cfg mdl =
    Grid.containerFluid []
        [ CDN.stylesheet
        , Grid.containerFluid [ class "page-header" ]
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ h1 [{- class "display-1" -}] [ text cfg.eventName ]
                    , p [] [ text "Mountain View Church" ]
                    ]
                ]
            ]
        , Grid.container []
            [ Grid.row []
                [ Grid.col [] [ errors mdl.errs ] ]
            , Grid.row []
                [ Grid.col [] [ personSearch ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ h1 [] [ text "Attending" ]
                    , listPeople mdl.checkedIn
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ h1 [] [ text "Found" ]
                    , listPeople mdl.foundPeople
                    ]
                ]
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
        Decode.map4
            Person
            (Decode.field "username" Decode.string)
            (Decode.field "email" Decode.string)
            (Decode.map toString (Decode.field "id" Decode.int))
            (Decode.succeed False)



-- TODO: remove as our API will have the
-- attendance data toggled


listPeople : List Person -> Html Msg
listPeople =
    List.map (person >> List.singleton >> ListGroup.li []) >> ListGroup.ul


person : Person -> Html Msg
person p =
    a [ onClick (ToggleAttending p.id) ]
        [ Grid.row []
            [ Grid.col [] [ text p.id ]
            , Grid.col [] [ text p.firstName ]
            , Grid.col [] [ text p.lastName ]
            , Grid.col [ Col.pushXs2 ]
                [ if p.attending then
                    (text "attending")
                  else
                    (text "not attending")
                ]
            ]
        ]
