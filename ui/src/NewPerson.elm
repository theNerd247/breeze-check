module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Data exposing (..)
import Dict as Dict
import Dict.Extra as DExtra
import Html as Html exposing (Html, br, text)
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
            [ Person.tableCellShort [ addButton mdl.newPerson ]
            , Person.tableCell
                [ Html.map PersonMsg <| Person.firstNameForm mdl.newPerson.personName.firstName
                ]
            , Person.tableCell
                [ Html.map PersonMsg <| Person.lastNameForm mdl.newPerson.personName.lastName
                ]
            ]
    in
    Form.form [] <|
        [ Person.editPersons
            |> Person.mapConfig PersonsMsg
            |> Person.setHeader header
            |> Person.view mdl.newPersons
        ]


newPersonInfoForm : HasNewPersons m -> Html Msg
newPersonInfoForm mdl =
    let
        personForm _ p =
            Grid.row []
                [ Grid.col []
                    [ Html.map PersonMsg <| Person.newPersonInfoForm p.newPersonInfo
                    ]
                ]
    in
    mdl.newPersons
        --|> Dict.values
        --|> DExtra.groupBy (.lastName << .personName)
        |> Dict.map personForm
        |> Dict.values
        |> Form.form []


addButton : Data.Person -> Html Msg
addButton mdl =
    Button.button
        [ Button.onClick CreateNewPerson
        , Button.success
        , Button.disabled <|
            String.isEmpty mdl.personName.firstName
                || String.isEmpty mdl.personName.lastName
        ]
        [ text "Add" ]


createAttendeesButton : Html Msg
createAttendeesButton =
    Button.button
        [ Button.onClick CreateNewAttendees
        , Button.outlineSuccess
        ]
        [ text "Edit Address" ]
