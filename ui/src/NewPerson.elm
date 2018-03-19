module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Data exposing (..)
import Dict as Dict
import Html as Html exposing (Html, br, text)
import Person as Person
import Set as Set


type alias HasNewPersons m =
    { m
        | newPersons : Person.Persons
        , newPerson : Data.Person
        , newPersonInfos : Dict.Dict Data.LastName Data.NewPersonInfo
    }


type Msg
    = PersonsMsg Person.PersonsMsg
    | PersonMsg Person.PersonMsg
    | CreateNewAttendees
    | NewPersonInfoMsg Person.NewPersonInfoMsg
    | NextNewInfo Data.LastName
    | PrevNewInfo Data.LastName


afterCreateNewAttendees : Msg -> HasNewPersons m -> HasNewPersons m -> HasNewPersons m
afterCreateNewAttendees msg b a =
    case msg of
        CreateNewAttendees ->
            b

        _ ->
            a


resetNewPersons : HasNewPersons m -> HasNewPersons m
resetNewPersons mdl =
    { mdl
        | newPersons = Dict.empty
    }


resetNewPersonInfos : HasNewPersons m -> HasNewPersons m
resetNewPersonInfos mdl =
    { mdl
        | newPersonInfos =
            mdl.newPersons
                |> Dict.values
                |> List.map (\p -> p.personName.lastName)
                |> Set.fromList
                |> Set.toList
                |> List.map (\l -> ( l, Person.initNewPersonInfo ))
                |> Dict.fromList
    }


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
            |> Person.view mdl.newPersons
            |> Html.map PersonsMsg
        ]


newPersonInfoForm : Data.LastName -> HasNewPersons m -> Html Msg
newPersonInfoForm k mdl =
    mdl.newPersonInfos
        |> Dict.get k
        |> Person.newPersonInfoForm
        |> Html.map NewPersonInfoMsg


createAttendeesButton : Html Msg
createAttendeesButton =
    Button.button
        [ Button.onClick CreateNewAttendees
        , Button.outlineSuccess
        ]
        [ text "Save" ]
