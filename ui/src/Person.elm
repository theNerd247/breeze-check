module Person exposing (..)

import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import BootstrapUtils as Utils
import Data as Data
import Dict as Dict exposing (Dict)
import Html as Html exposing (Html, div, text)


type alias Persons =
    Dict Data.PersonId Data.Person


type PersonsMsg
    = Create Data.Person
    | Replace Data.Person
    | Update Data.PersonId PersonMsg
    | Delete Data.PersonId


type PersonMsg
    = SetLastName String
    | SetFirstName String
    | SetName Data.Name
    | SetCheckedIn Data.CheckInStatus
    | SetNewPersonInfo Data.NewPersonInfo
    | SetWantsPhotos Bool


type NewPersonInfoMsg
    = UpdateAddress AddressMsg
    | UpdateEmail String
    | UpdateCurrentChurch String


type AddressMsg
    = UpdateStreet String
    | UpdateCity String
    | UpdateState String
    | UpdateZip String


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


editPersons : Config PersonsMsg
editPersons =
    let
        setNameView ph f g p =
            Input.text
                [ Input.onInput <| Update p.pid << f
                , Input.value <| g p
                , Input.placeholder ph
                ]

        delCol p =
            Utils.deleteButton <| Delete p.pid
    in
    config
        |> firstNameView (setNameView "First Name" SetFirstName (.personName >> .firstName))
        |> lastNameView (setNameView "Last Name" SetLastName (.personName >> .lastName))
        |> lastCol delCol


selectPersons : (Bool -> PersonMsg) -> Config PersonsMsg
selectPersons f =
    let
        selPerson p =
            let
                personChecked =
                    case p.checkedIn of
                        Data.SelectedForCheckIn ->
                            True

                        _ ->
                            False
            in
            Checkbox.checkbox
                [ Checkbox.id <| toString p.pid
                , Checkbox.checked personChecked
                , Checkbox.onCheck <| Update p.pid << f
                ]
                ""
    in
    config
        |> lastCol selPerson


selectPersonsForCheckIn : Config PersonsMsg
selectPersonsForCheckIn =
    let
        setChecked b =
            if b then
                Data.SelectedForCheckIn
            else
                Data.CheckedOut
    in
    selectPersons <| SetCheckedIn << setChecked


selectPersonsForWantsPhotos : Config PersonsMsg
selectPersonsForWantsPhotos =
    selectPersons SetWantsPhotos


onlyListPersons : Persons -> Html msg
onlyListPersons =
    flip view config


initPersons : Persons
initPersons =
    Dict.empty


initPerson : Data.Person
initPerson =
    let
        address =
            { street = ""
            , city = ""
            , state = ""
            , zipcode = ""
            }

        newPersonInfo =
            { newAddress = address
            , newCurrentChurch = ""
            , newEmail = ""
            }
    in
    { pid = 0
    , personName = { lastName = "", firstName = "" }
    , checkedIn = Data.CheckedOut
    , newPersonInfo = Just newPersonInfo
    , wantsPhotos = True
    }


updatePersons : PersonsMsg -> Persons -> Persons
updatePersons msg mdl =
    case msg of
        Create p ->
            Dict.insert p.pid p mdl

        Replace p ->
            Dict.update p.pid (Maybe.map <| always p) mdl

        Update pid msg ->
            Dict.update pid (Maybe.map <| updatePerson msg) mdl

        Delete pid ->
            Dict.remove pid mdl


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

        SetWantsPhotos b ->
            { mdl | wantsPhotos = b }


updateNewPersonInfo : NewPersonInfoMsg -> Data.NewPersonInfo -> Data.NewPersonInfo
updateNewPersonInfo msg mdl =
    case msg of
        UpdateAddress m ->
            { mdl | newAddress = updateAddress m mdl.newAddress }

        UpdateEmail e ->
            { mdl | newEmail = e }

        UpdateCurrentChurch c ->
            { mdl | newCurrentChurch = c }


updateAddress : AddressMsg -> Data.Address -> Data.Address
updateAddress msg mdl =
    case msg of
        UpdateStreet s ->
            { mdl | street = s }

        UpdateCity c ->
            { mdl | city = c }

        UpdateState s ->
            { mdl | state = s }

        UpdateZip z ->
            { mdl | zipcode = z }


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
        , Table.td [] [ cfg.lastNameView p ]
        , Table.td [] <|
            case cfg.lastCol of
                Nothing ->
                    []

                Just f ->
                    [ f p ]
        ]
