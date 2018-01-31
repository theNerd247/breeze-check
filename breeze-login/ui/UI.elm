module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
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
    | FoundPeople (Result Http.Error (Result Breeze.BreezeException (List Breeze.Person)))
    | ToggleAttending String
    | ErrorMessage Err.Msg
    | NewPersonSelect


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
    , apiBase = "" -- "http://10.0.0.100:8080"
    , debug = False
    }


newError : String -> Model -> Model
newError msg mdl =
    { mdl | errors = Err.newError msg mdl.errors }


catchResult : (b -> String) -> (a -> Model -> Model) -> Result b a -> Model -> Model
catchResult g f err =
    case err of
        Err e ->
            newError (g e)

        Ok a ->
            f a


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update cfg msg mdl =
    case msg of
        UpdateLastName s ->
            ( { mdl | searchLastName = s }, Cmd.none )

        LastNameSearch ->
            ( mdl, findPeople cfg mdl.searchLastName )

        FoundPeople r ->
            ( mdl
                |> (r
                        |> catchResult toString
                            (catchResult .breezeErr
                                updateFoundPeople
                            )
                   )
            , Cmd.none
            )

        ToggleAttending pid ->
            let
                ( chin, fnd ) =
                    toggleCheckIn pid ( mdl.checkedIn, mdl.foundPeople )
            in
            ( { mdl | checkedIn = chin, foundPeople = fnd }, Cmd.none )

        ErrorMessage emsg ->
            ( { mdl | errors = Err.update emsg mdl.errors }, Cmd.none )

        NewPersonSelect ->
            ( mdl, Cmd.none )


updateFoundPeople : List Breeze.Person -> Model -> Model
updateFoundPeople ps mdl =
    if List.isEmpty ps then
        mdl
            |> (newError <|
                    "I'm sorry there's no one with the last name of: "
                        ++ mdl.searchLastName
               )
    else
        { mdl | foundPeople = ps }


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
    let
        includeCDN =
            if cfg.debug then
                [ CDN.stylesheet ]
            else
                []
    in
    Grid.container [] <|
        List.append
            includeCDN
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ h1 [{- class "display-1" -}] [ text cfg.eventName ]
                    , p [] [ text "Mountain View Church" ]
                    ]
                ]
            , Grid.row []
                [ Grid.col [] [ Html.map ErrorMessage <| Err.view mdl.errors ] ]
            , Grid.row []
                [ Grid.col []
                    [ h4 [] [ text "Find Your Family..." ]
                    , personSearch
                    ]
                ]
            , Grid.row [] <|
                if List.isEmpty mdl.foundPeople then
                    []
                else
                    [ Grid.col [ Col.xs12 ]
                        [ h4 [] [ text "Select Members To Check In" ]
                        , listPeople mdl.foundPeople
                        ]
                    ]
            , Grid.row [] <|
                if List.isEmpty mdl.checkedIn then
                    []
                else
                    [ Grid.col [ Col.xs12 ]
                        [ h4 [] [ text "Checked In" ]
                        , listPeople mdl.checkedIn
                        ]
                    ]
            ]


personSearch : Html Msg
personSearch =
    Form.form []
        [ Form.row []
            [ Form.col []
                [ InputGroup.config
                    (InputGroup.text [ Input.placeholder "Last Name", Input.onInput UpdateLastName ])
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.success
                            , Button.large
                            , Button.onClick LastNameSearch
                            ]
                            [ Html.i
                                [ class "fas fa-search"
                                ]
                                []
                            ]
                        ]
                    |> InputGroup.large
                    |> InputGroup.view
                ]
            ]
        , Form.row [ Row.centerXs ]
            [ Form.col [ Col.xsAuto ]
                [ Button.button
                    [ Button.primary
                    , Button.large
                    , Button.onClick NewPersonSelect
                    ]
                    [ text "I can't find us" ]
                ]
            ]
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
                , timeout = Just 3000
                , withCredentials = False
                }
    in
    Http.send f req


withBreezeErrDecoder : Decode.Decoder a -> Decode.Decoder (Result Breeze.BreezeException a)
withBreezeErrDecoder d =
    Decode.oneOf
        [ Decode.map Err Breeze.decodeBreezeException
        , Decode.map Ok d
        ]


findPeople : Config -> String -> Cmd Msg
findPeople cfg lastName =
    sendApiGetRequest cfg
        ("findperson?lastname=" ++ lastName)
        (withBreezeErrDecoder decodePersons)
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
        [ Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [ Col.xs5 ] [ text p.firstName ]
                , Grid.col [ Col.xs5 ] [ text p.lastName ]
                , Grid.col [ Col.xs2, Col.pushXs5 ] <|
                    if p.checkedIn then
                        [ Html.i [ class "fas fa-check" ] [] ]
                    else
                        []
                ]
            ]
        ]
