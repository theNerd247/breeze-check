module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Data exposing (..)
import Dict as Dict
import Html as Html exposing (Html, br, text)
import Person as Person


type alias HasNewPersons m =
    { m | newPersons : Person.Persons, newPerson : Data.Person }


type Msg
    = PersonsMsg Person.PersonsMsg
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
        PersonMsg msg ->
            { mdl | newPerson = Person.updatePerson msg mdl.newPerson }

        PersonsMsg msg ->
            case msg of
                Person.PersonMsg m ->
                    { mdl | newPerson = Person.updatePerson m mdl.newPerson }

                Person.Create np ->
                    let
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

                _ ->
                    { mdl | newPersons = Person.updatePersons msg mdl.newPersons }

        _ ->
            mdl


newPersonsForm : HasNewPersons m -> Html Msg
newPersonsForm mdl =
    Form.form [] <|
        [ Person.editPersons mdl.newPerson
            |> Person.mapConfig PersonsMsg
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


createAttendeesButton : Html Msg
createAttendeesButton =
    Button.button
        [ Button.onClick CreateNewAttendees
        , Button.outlineSuccess
        ]
        [ text "Edit Address" ]
