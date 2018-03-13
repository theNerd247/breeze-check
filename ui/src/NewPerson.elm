module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Data exposing (..)
import Dict as Dict
import Html as Html exposing (Html, text)
import Person as Person


type alias HasNewPersons m =
    { m | newPersons : Person.Persons, newPerson : Data.Person }


type Msg
    = CreateNewPerson
    | PersonsMsg Person.PersonsMsg
    | PersonMsg Person.PersonMsg


update : Msg -> HasNewPersons m -> HasNewPersons m
update msg mdl =
    case msg of
        CreateNewPerson ->
            let
                np =
                    mdl.newPerson

                pid =
                    mdl.newPersons
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.map (\x -> x + 1)
                        |> Maybe.withDefault 0
            in
            { mdl
                | newPersons =
                    Dict.insert (Debug.log "pid: " pid)
                        { np | pid = pid }
                        mdl.newPersons
                , newPerson = Person.initPerson
            }

        PersonMsg msg ->
            { mdl | newPerson = Person.updatePerson msg mdl.newPerson }

        PersonsMsg msg ->
            { mdl | newPersons = Person.updatePersons msg mdl.newPersons }


newPersonsForm : HasNewPersons m -> Html Msg
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
