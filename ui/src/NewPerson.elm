module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Data exposing (..)
import Dict as Dict
import Dict.Extra as DExtra
import Html as Html exposing (Html, br, text)
import List.Zipper as Zipper exposing (Zipper)
import Person as Person


type alias LastNamesIndex =
    { lastNames : Zipper Data.LastName
    , lastKey : Data.LastName
    , firstKey : Data.LastName
    }


type alias NPInfoIndex =
    { npids : List Data.PersonId
    , npInfo : Data.NewPersonInfo
    }


type alias NPIDict =
    Dict.Dict Data.LastName NPInfoIndex


type alias HasNewPersons m =
    { m
        | newPersons : Person.Persons
        , newPerson : Data.Person
        , newPersonInfos : NPIDict
        , lastNamesIndex : LastNamesIndex
    }


type alias NPIMsg =
    Person.DictMsg Person.NewPersonInfoMsg Data.LastName NPInfoIndex


type Msg
    = PersonsMsg Person.PersonsMsg
    | PersonMsg Person.PersonMsg
    | CreateNewAttendees
    | NPIMsg NPIMsg
    | NextNewInfo
    | PrevNewInfo


initNewPersonInfos : Dict.Dict Data.LastName NPInfoIndex
initNewPersonInfos =
    Dict.empty


initLastNamesIndex : LastNamesIndex
initLastNamesIndex =
    { lastNames = Zipper.singleton ""
    , lastKey = ""
    , firstKey = ""
    }


guardCreateNewAttendess : Msg -> HasNewPersons m -> HasNewPersons m -> HasNewPersons m
guardCreateNewAttendess msg b a =
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
    let
        mkindex _ ps =
            { npids = ps |> List.map .pid
            , npInfo = Person.initNewPersonInfo
            }

        npis =
            mdl.newPersons
                |> Dict.values
                |> DExtra.groupBy (\p -> p.personName.lastName)
                |> Dict.map mkindex

        lnames =
            Dict.keys npis
                |> Zipper.fromList
                |> Zipper.withDefault ""

        lkey =
            lnames
                |> Zipper.last
                |> Zipper.current

        fkey =
            lnames
                |> Zipper.first
                |> Zipper.current
    in
    { mdl
        | newPersonInfos = npis
        , lastNamesIndex = { lastNames = lnames, lastKey = lkey, firstKey = fkey }
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

                Person.Create _ np ->
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

        CreateNewAttendees ->
            let
                setNewPersons _ idx nps =
                    let
                        setNewPerson pid ns =
                            Dict.update pid (Maybe.map <| \p -> { p | newPersonInfo = Just idx.npInfo }) ns
                    in
                    idx.npids
                        |> List.foldl setNewPerson nps
            in
            { mdl
                | newPersons =
                    mdl.newPersonInfos
                        |> Dict.foldl setNewPersons
                            mdl.newPersons
            }

        NextNewInfo ->
            let
                l =
                    mdl.lastNamesIndex

                lnames =
                    { l
                        | lastNames =
                            Zipper.next mdl.lastNamesIndex.lastNames
                                |> Maybe.withDefault
                                    mdl.lastNamesIndex.lastNames
                    }
            in
            { mdl | lastNamesIndex = lnames }

        PrevNewInfo ->
            let
                l =
                    mdl.lastNamesIndex

                lnames =
                    { l
                        | lastNames =
                            Zipper.next mdl.lastNamesIndex.lastNames
                                |> Maybe.withDefault
                                    mdl.lastNamesIndex.lastNames
                    }
            in
            { mdl | lastNamesIndex = lnames }

        NPIMsg m ->
            { mdl | newPersonInfos = updateNPI m mdl.newPersonInfos }


updateNPI : NPIMsg -> NPIDict -> NPIDict
updateNPI =
    let
        npiWrapper mg mdl =
            { mdl
                | npInfo = Person.updateNewPersonInfo mg mdl.npInfo
            }
    in
    Person.updateDict npiWrapper


newPersonsForm : HasNewPersons m -> Html Msg
newPersonsForm mdl =
    Form.form [] <|
        [ Person.editPersons mdl.newPerson
            |> Person.view mdl.newPersons
            |> Html.map PersonsMsg
        ]


newPersonInfoForm : HasNewPersons m -> Html Msg
newPersonInfoForm mdl =
    let
        curName =
            Zipper.current mdl.lastNamesIndex.lastNames
    in
    mdl.newPersonInfos
        |> Dict.get curName
        |> Maybe.map .npInfo
        |> Person.newPersonInfoForm
        |> Html.map (NPIMsg << Person.Update curName)


createAttendeesButton : Html Msg
createAttendeesButton =
    Button.button
        [ Button.onClick CreateNewAttendees
        , Button.outlineSuccess
        ]
        [ text "Save" ]
