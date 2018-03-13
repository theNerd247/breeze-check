module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Data exposing (..)
import Dict as Dict
import Html as Html exposing (Html, text)
import Html.Attributes exposing (class)
import Person as Person


type alias HasNewPersons m =
    { m | newPersons : Person.Persons, newPerson : Data.Person }


type Msg
    = CreateNewPerson
    | PersonsMsg Person.PersonsMsg
    | PersonMsg Person.PersonMsg
    | CreateNewAttendees


afterCreateNewAttendees : Msg -> HasNewPersons m -> HasNewPersons m -> HasNewPersons m
afterCreateNewAttendees msg b a =
    case msg of
        CreateNewAttendees ->
            b

        _ ->
            a


update : Msg -> HasNewPersons m -> HasNewPersons m
update msg mdl =
    case msg of
        CreateNewPerson ->
            let
                np =
                    mdl.newPerson

                pid =
                    np.pid + 1

                ip =
                    Person.initPerson
            in
            { mdl
                | newPersons =
                    Dict.insert pid
                        { np | pid = pid, checkedIn = Data.SelectedForCheckIn }
                        mdl.newPersons
                , newPerson = { ip | pid = pid }
            }

        PersonMsg msg ->
            { mdl | newPerson = Person.updatePerson msg mdl.newPerson }

        PersonsMsg msg ->
            { mdl | newPersons = Person.updatePersons msg mdl.newPersons }

        _ ->
            mdl


newPersonsForm : HasNewPersons m -> Html Msg
newPersonsForm mdl =
    let
        header =
            [ Person.tableCellShort [ addButton ]
            , Person.tableCell
                [ Person.editName "First Name"
                    mdl.newPerson.personName.firstName
                    (PersonMsg << Person.SetFirstName)
                ]
            , Person.tableCell
                [ Person.editName "Last Name"
                    mdl.newPerson.personName.lastName
                    (PersonMsg << Person.SetLastName)
                ]
            ]
    in
    Form.form []
        [ Person.editPersons
            |> Person.mapConfig PersonsMsg
            |> Person.setHeader header
            |> Person.view mdl.newPersons
        , createAttendeesButton
        ]


addButton : Html Msg
addButton =
    Button.button
        [ Button.onClick CreateNewPerson
        , Button.success
        ]
        [ Html.i [ class "fas fa-user-plus" ] [] ]


createAttendeesButton : Html Msg
createAttendeesButton =
    Button.button
        [ Button.onClick CreateNewAttendees
        , Button.success
        ]
        [ text "Done" ]
