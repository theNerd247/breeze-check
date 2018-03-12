module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Data exposing (..)
import Dict as Dict
import Html as Html exposing (Html, text)
import Person as Person


type alias HasNewPersons m =
    { m | newPersons : Person.Persons }


type alias Model =
    HasNewPersons { newPerson : Data.Person }


type Msg
    = CreateNewPerson
    | PersonsMsg Person.PersonsMsg
    | PersonMsg Person.PersonMsg


update : Msg -> Model -> Model
update msg mdl =
    case msg of
        CreateNewPerson ->
            let
                np =
                    mdl.newPersons

                pid =
                    mdl.newPersons
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> (\x -> x + 1)
            in
            { mdl
                | newPersons = Dict.insert pid mdl.newPerson mdl.newPersons
                , newPerson = Person.initPerson
            }

        PersonMsg msg ->
            { mdl | newPerson = Person.updatePerson msg mdl.newPerson }

        PersonsMsg msg ->
            { mdl | newPersons = Person.updatePersons msg mdl.newPersons }


newPersonsForm : Model -> Html Msg
newPersonsForm mdl =
    Form.form []
        [ addButton
        , Html.map PersonsMsg <| Person.view mdl.newPersons Person.editPersons
        ]


addButton : Html Msg
addButton =
    Button.button
        [ Button.onClick CreateNewPerson
        , Button.success
        ]
        [ text "Add Person" ]
