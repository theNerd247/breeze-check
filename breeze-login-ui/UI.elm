module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
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
        , button
        , div
        , h1
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
import Json.Decode as Decode


type alias Model =
    { searchLastName : String
    , foundPeople : List Breeze.Person
    , checkedIn : List Breeze.Person
    , errors : Err.Errors
    }


type alias Config =
    { eventName : String
    , apiBase : String
    , debug : Bool
    }


type Msg
    = LastNameSearch
    | UpdateLastName String
    | FoundPeople (Result Http.Error (List Breeze.Person))
    | ToggleAttending String
    | ErrorMessage Err.Msg


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
    }


config : Config
config =
    { eventName = "Test Event"
    , apiBase = "http://10.0.0.100:8080"
    , debug = True
    }


newError : Model -> String -> Model
newError mdl msg =
    { mdl | errors = Err.newError msg mdl.errors }


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update cfg msg mdl =
    case msg of
        UpdateLastName s ->
            ( { mdl | searchLastName = s }, Cmd.none )

        LastNameSearch ->
            ( mdl, findPeople cfg mdl.searchLastName )

        FoundPeople (Ok []) ->
            ( newError mdl <| "I'm sorry nobody exists with the last name of: " ++ mdl.searchLastName, Cmd.none )

        FoundPeople (Ok ps) ->
            ( { mdl | foundPeople = ps }, Cmd.none )

        FoundPeople (Err e) ->
            ( newError mdl (toString e), Cmd.none )

        ToggleAttending pid ->
            let
                ( chin, fnd ) =
                    toggleCheckIn pid ( mdl.checkedIn, mdl.foundPeople )
            in
            ( { mdl | checkedIn = chin, foundPeople = fnd }, Cmd.none )

        ErrorMessage emsg ->
            ( { mdl | errors = Err.update emsg mdl.errors }, Cmd.none )


toggleCheckIn : String -> ( List Breeze.Person, List Breeze.Person ) -> ( List Breeze.Person, List Breeze.Person )
toggleCheckIn pid ( chkin, found ) =
    let
        personFilter p =
            p.pid == pid

        toggleAttend p =
            { p | checkedIn = not p.checkedIn }

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
                [ Grid.col [] [ Html.map ErrorMessage <| Err.view mdl.errors ] ]
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


sendApiGetRequest : Config -> String -> Decode.Decoder a -> (Result Http.Error a -> Msg) -> Cmd Msg
sendApiGetRequest cfg path decoder f =
    let
        req =
            Http.request
                { method = "GET"
                , headers = []
                , url = cfg.apiBase ++ path
                , body = Http.emptyBody
                , expect = Http.expectJson decoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send f req


findPeople : Config -> String -> Cmd Msg
findPeople cfg lastName =
    sendApiGetRequest cfg
        ("/findperson?lastname=" ++ lastName)
        decodePersons
        FoundPeople


decodePersons : Decode.Decoder (List Breeze.Person)
decodePersons =
    Decode.list Breeze.decodePerson



-- TODO: remove as our API will have the
-- attendance data toggled


listPeople : List Breeze.Person -> Html Msg
listPeople =
    List.map (person >> List.singleton >> ListGroup.li []) >> ListGroup.ul


person : Breeze.Person -> Html Msg
person p =
    a [ onClick (ToggleAttending p.pid) ]
        [ Grid.row []
            [ Grid.col [] [ text p.pid ]
            , Grid.col [] [ text p.firstName ]
            , Grid.col [] [ text p.lastName ]
            , Grid.col [ Col.pushXs2 ]
                [ if p.checkedIn then
                    text "attending"
                  else
                    text "not attending"
                ]
            ]
        ]
