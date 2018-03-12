module Person exposing (..)

import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import BootstrapUtils as Utils
import Data as Data
import Dict as Dict exposing (Dict)
import Html as Html exposing (Html, div, text)


type alias PersonId =
    String


type alias Persons =
    Dict PersonId Data.Person


type Msg
    = Create Data.Person
    | Replace Data.Person
    | Update PersonId PersonMsg
    | Delete PersonId


type PersonMsg
    = SetLastName String
    | SetFirstName String
    | SetName Data.Name
    | SetCheckedIn Data.CheckInStatus
    | SetNewPersonInfo Data.NewPersonInfo


type alias Config msg =
    { lastCol : Maybe (Data.Person -> Html msg)
    , lastNameView : Data.Person -> Html msg
    , firstNameView : Data.Person -> Html msg
    }


config : Config msg
config =
    { lastCol = Nothing
    , lastNameView = \p -> text p.personName.lastName
    , firstNameView = \p -> text p.personName.firstName
    }


lastCol : (Data.Person -> Html msg) -> Config msg -> Config msg
lastCol f mdl =
    { mdl | lastCol = Just f }


lastNameView : (Data.Person -> Html msg) -> Config msg -> Config msg
lastNameView f mdl =
    { mdl | lastNameView = f }


firstNameView : (Data.Person -> Html msg) -> Config msg -> Config msg
firstNameView f mdl =
    { mdl | firstNameView = f }


onlyListPersons : Persons -> Html Msg
onlyListPersons =
    flip view config


update : Msg -> Persons -> ( Persons, Cmd Msg )
update msg mdl =
    case msg of
        Create p ->
            ( Dict.insert p.pid p mdl, Cmd.none )

        Replace p ->
            ( Dict.update p.pid (Maybe.map <| always p) mdl, Cmd.none )

        Update pid msg ->
            ( Dict.update pid (Maybe.map <| updatePerson msg) mdl, Cmd.none )

        Delete pid ->
            ( Dict.remove pid mdl, Cmd.none )


updatePerson : PersonMsg -> Data.Person -> Data.Person
updatePerson msg mdl =
    case msg of
        SetLastName l ->
            let
                n =
                    mdl.personName
            in
            { mdl | personName = { n | lastName = l } }

        SetFirstName fn ->
            let
                n =
                    mdl.personName
            in
            { mdl | personName = { n | firstName = fn } }

        SetName n ->
            { mdl | personName = n }

        SetCheckedIn c ->
            { mdl | checkedIn = c }

        SetNewPersonInfo np ->
            { mdl | newPersonInfo = Just np }


view : Persons -> Config msg -> Html msg
view ps config =
    let
        head =
            Table.simpleThead []

        body =
            Dict.values ps
                |> List.map (personRow config)
                |> Table.tbody []
    in
    Table.simpleTable ( head, body )


personRow : Config msg -> Data.Person -> Table.Row msg
personRow cfg p =
    Table.tr []
        [ Table.td [] [ cfg.firstNameView p ]
        , Table.td [] [ cfg.firstNameView p ]
        , Table.td [] <|
            case cfg.lastCol of
                Nothing ->
                    []

                Just f ->
                    [ f p ]
        ]


viewForm : Config Msg
viewForm =
    let
        setNameView f g p =
            Input.text
                [ Input.onInput <| Update p.pid << f
                , Input.value <| g p
                ]

        delCol p =
            Utils.deleteButton <| Delete p.pid
    in
    config
        |> firstNameView (setNameView SetFirstName (.personName >> .firstName))
        |> lastNameView (setNameView SetLastName (.personName >> .lastName))
        |> lastCol delCol
