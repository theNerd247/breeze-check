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


resetNewPersonInfos : Person.Persons -> HasNewPersons m -> HasNewPersons m
resetNewPersonInfos ps mdl =
    let
        -- instead of using a new data set everytime update it from the list of
        -- peoples last names if possible
        getNewPersonInfo =
            List.filterMap .newPersonInfo
                >> List.head
                >> Maybe.withDefault Person.initNewPersonInfo

        mkindex _ ps =
            { npids = ps |> List.map .pid
            , npInfo = getNewPersonInfo ps
            }

        lls =
            mdl.newPersons |> Dict.values |> List.map (.personName >> .lastName >> String.toLower)

        nps =
            -- grab everyone we're checking in by last name and see if the user
            -- is trying to add someone by that last name. Then update everyone
            -- who has that lastname
            Dict.union
                (ps
                    |> Dict.filter
                        (\_ x ->
                            List.member
                                (String.toLower x.personName.lastName)
                                lls
                        )
                )
                mdl.newPersons

        npis =
            nps
                |> Dict.values
                |> DExtra.groupBy (.personName >> .lastName >> String.toLower)
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
        | newPersons = nps
        , newPersonInfos = npis
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
                    resetNewPersonInfos Dict.empty
                        { mdl
                            | newPersons =
                                Dict.insert pid
                                    { np | pid = pid, checkedIn = Data.SelectedForCheckIn }
                                    mdl.newPersons
                            , newPerson = { ip | pid = pid }
                        }

                Person.Delete ix ->
                    resetNewPersonInfos Dict.empty
                        { mdl
                            | newPersons = Person.updatePersons msg mdl.newPersons
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
                            Zipper.previous mdl.lastNamesIndex.lastNames
                                |> Maybe.withDefault
                                    mdl.lastNamesIndex.lastNames
                    }
            in
            { mdl | lastNamesIndex = lnames }

        NPIMsg m ->
            { mdl | newPersonInfos = updateNPI m mdl.newPersonInfos }


formatName : String -> String
formatName s =
    String.toUpper (String.left 1 s) ++ (String.toLower <| String.dropLeft 1 s)


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
    Form.form []
        [ Person.editPersons mdl.newPerson
            |> Person.view mdl.newPersons
            |> Html.map PersonsMsg
        ]


newPersonReview : HasNewPersons m -> Html Msg
newPersonReview mdl =
    Form.form []
        [ mdl.newPersons
            |> Person.onlyListPersons
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
        |> Maybe.map (npiForm curName)
        |> Maybe.withDefault (text "")


npiForm : LastName -> NPInfoIndex -> Html Msg
npiForm curName mdl =
    mdl
        |> .npInfo
        |> Person.newPersonInfoForm
        |> Html.map (NPIMsg << Person.Update curName)


createAttendeesButton : Html Msg
createAttendeesButton =
    Button.button
        [ Button.onClick CreateNewAttendees
        , Button.outlineSuccess
        ]
        [ text "Save" ]
