module Person exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table
import Data as Data
import Dict as Dict exposing (Dict)
import Html as Html exposing (Html, div, text)
import Html.Attributes exposing (class, colspan)


type alias Persons =
    Dict Data.PersonId Data.Person


type PersonsMsg
    = Create Data.Person
    | Replace Data.Person
    | Update Data.PersonId PersonMsg
    | Delete Data.PersonId
    | UpdateAll PersonMsg


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
    { extraCol : Maybe (Data.Person -> Html msg)
    , lastNameView : Data.Person -> Html msg
    , firstNameView : Data.Person -> Html msg
    , rowOptions : Data.Person -> List (Table.RowOption msg)
    , selectAll : Maybe (Bool -> msg)
    }


config : Config msg
config =
    { extraCol = Nothing
    , lastNameView = \p -> text p.personName.lastName
    , firstNameView = \p -> text p.personName.firstName
    , rowOptions = always []
    , selectAll = Nothing
    }


extraCol : (Data.Person -> Html msg) -> Config msg -> Config msg
extraCol f mdl =
    { mdl | extraCol = Just f }


lastNameView : (Data.Person -> Html msg) -> Config msg -> Config msg
lastNameView f mdl =
    { mdl | lastNameView = f }


firstNameView : (Data.Person -> Html msg) -> Config msg -> Config msg
firstNameView f mdl =
    { mdl | firstNameView = f }


rowOps : (Data.Person -> List (Table.RowOption msg)) -> Config msg -> Config msg
rowOps f cfg =
    { cfg | rowOptions = f }


allowSelectAll : (Bool -> msg) -> Config msg -> Config msg
allowSelectAll f mdl =
    { mdl | selectAll = Just f }


editPersons : Config PersonsMsg
editPersons =
    let
        setNameView ph f g p =
            Input.text
                [ Input.onInput <| Update p.pid << f
                , Input.value <| g p
                , Input.placeholder ph
                ]

        deleteButton p =
            Button.button
                [ Button.onClick <| Delete p.pid
                , Button.danger
                , Button.roleLink
                ]
                [ Html.i [ class "far fa-trash-alt text-danger" ] [] ]
    in
    config
        |> firstNameView (setNameView "First Name" SetFirstName (.personName >> .firstName))
        |> lastNameView (setNameView "Last Name" SetLastName (.personName >> .lastName))
        |> extraCol deleteButton


selectPersons : (Bool -> PersonMsg) -> (Data.Person -> Bool) -> Config PersonsMsg
selectPersons f personChecked =
    let
        selPerson p =
            Checkbox.custom
                [ Checkbox.id <| toString p.pid
                , Checkbox.checked <| personChecked p
                , Checkbox.onCheck <| Update p.pid << f
                ]
                ""
    in
    config
        |> extraCol selPerson


selectPersonsForCheckIn : Config PersonsMsg
selectPersonsForCheckIn =
    let
        setChecked b =
            if b then
                Data.SelectedForCheckIn
            else
                Data.CheckedOut

        personChecked p =
            case p.checkedIn of
                Data.SelectedForCheckIn ->
                    True

                _ ->
                    False

        setActive p =
            if p.checkedIn == Data.SelectedForCheckIn then
                [ Table.rowSuccess
                ]
            else
                []

        m =
            SetCheckedIn << setChecked
    in
    selectPersons m personChecked
        |> rowOps setActive
        |> allowSelectAll (UpdateAll << m)


selectPersonsForWantsPhotos : Config PersonsMsg
selectPersonsForWantsPhotos =
    selectPersons SetWantsPhotos .wantsPhotos
        |> allowSelectAll (UpdateAll << SetWantsPhotos)


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

        UpdateAll f ->
            Dict.map (\_ p -> updatePerson f p) mdl


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


tableCell =
    Table.td [ Table.cellAttr <| class "d-inline-block col-5" ]


tableCellShort =
    Table.td [ Table.cellAttr <| class "d-inline-block col-2" ]


view : Persons -> Config msg -> Html msg
view ps config =
    let
        head =
            Table.simpleThead <|
                if not <| Dict.isEmpty ps then
                    case config.selectAll of
                        Just f ->
                            [ Table.td [ Table.cellAttr <| colspan 3 ]
                                [ Checkbox.custom
                                    [ Checkbox.onCheck f
                                    , Checkbox.id "selectAll"
                                    , Checkbox.inline
                                    ]
                                    "Select All"
                                ]
                            ]

                        _ ->
                            []
                else
                    []

        body =
            Dict.values ps
                |> List.map (personRow config)
                |> Table.tbody []
    in
    Table.table
        { thead = head
        , tbody = body
        , options = [ Table.hover, Table.striped ]
        }


personRow : Config msg -> Data.Person -> Table.Row msg
personRow cfg p =
    Table.tr
        (cfg.rowOptions p)
        [ tableCellShort <|
            case cfg.extraCol of
                Nothing ->
                    []

                Just f ->
                    [ f p ]
        , tableCell [ cfg.firstNameView p ]
        , tableCell [ cfg.lastNameView p ]
        ]
