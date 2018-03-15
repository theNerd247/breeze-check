module Person exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
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
    = UpdateLastName String
    | UpdateFirstName String
    | UpdateName Data.Name
    | UpdateCheckedIn Data.CheckInStatus
    | UpdateNewPersonInfo NewPersonInfoMsg
    | UpdateWantsPhotos Bool


type NewPersonInfoMsg
    = UpdateAddress AddressMsg
    | UpdateEmail String
    | UpdateCurrentChurch (Maybe String)


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
    , header : List (Table.Cell msg)
    }


config : Config msg
config =
    { extraCol = Nothing
    , lastNameView = \p -> text p.personName.lastName
    , firstNameView = \p -> text p.personName.firstName
    , rowOptions = always []
    , header = []
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



-- | If you are using map make sure to set this AFTER the map is performed so
-- the resulting typ is mapped correctly. This is due to Table.RowOptions not
-- being a functor


rowOps : (Data.Person -> List (Table.RowOption msg)) -> Config msg -> Config msg
rowOps f cfg =
    { cfg | rowOptions = f }


setHeader : List (Table.Cell msg) -> Config msg -> Config msg
setHeader f mdl =
    { mdl | header = f }


mapConfig : (a -> b) -> Config a -> Config b
mapConfig f mdl =
    { mdl
        | extraCol = Maybe.map (\g -> Html.map f << g) mdl.extraCol
        , lastNameView = Html.map f << mdl.lastNameView
        , firstNameView = Html.map f << mdl.firstNameView
        , rowOptions = always []
        , header = []
    }


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
        |> firstNameView (setNameView "First Name" UpdateFirstName (.personName >> .firstName))
        |> lastNameView (setNameView "Last Name" UpdateLastName (.personName >> .lastName))
        |> extraCol deleteButton


selectPersons : (Bool -> PersonMsg) -> (Data.Person -> Bool) -> (Bool -> PersonsMsg) -> Config PersonsMsg
selectPersons f personChecked selectAllMsg =
    let
        selheader =
            [ Table.td [ Table.cellAttr <| colspan 3 ]
                [ Checkbox.custom
                    [ Checkbox.onCheck selectAllMsg
                    , Checkbox.id "selectAll"
                    , Checkbox.inline
                    ]
                    "Select All"
                ]
            ]

        selPerson p =
            Checkbox.custom
                [ Checkbox.id <| toString p.pid
                , Checkbox.checked <| personChecked p
                , Checkbox.onCheck <| Update p.pid << f
                ]
                ""

        setActive p =
            if personChecked p then
                [ Table.rowSuccess
                ]
            else
                []
    in
    config
        |> extraCol selPerson
        |> setHeader selheader
        |> rowOps setActive


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

        m =
            UpdateCheckedIn << setChecked
    in
    selectPersons m personChecked (UpdateAll << m)


selectPersonsForWantsPhotos : Config PersonsMsg
selectPersonsForWantsPhotos =
    selectPersons UpdateWantsPhotos .wantsPhotos (UpdateAll << UpdateWantsPhotos)


onlyListPersons : Persons -> Html msg
onlyListPersons =
    flip view config


initPersons : Persons
initPersons =
    Dict.empty


initNewPersonInfo : Data.NewPersonInfo
initNewPersonInfo =
    let
        address =
            { street = ""
            , city = ""
            , state = ""
            , zipcode = ""
            }
    in
    { newAddress = address
    , newCurrentChurch = Nothing
    , newEmail = ""
    }


initPerson : Data.Person
initPerson =
    { pid = 0
    , personName = { lastName = "", firstName = "" }
    , checkedIn = Data.CheckedOut
    , newPersonInfo = Just initNewPersonInfo
    , wantsPhotos = False
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
        UpdateLastName l ->
            let
                n =
                    mdl.personName
            in
            { mdl | personName = { n | lastName = l } }

        UpdateFirstName fn ->
            let
                n =
                    mdl.personName
            in
            { mdl | personName = { n | firstName = fn } }

        UpdateName n ->
            { mdl | personName = n }

        UpdateCheckedIn c ->
            { mdl | checkedIn = c }

        UpdateNewPersonInfo np ->
            { mdl
                | newPersonInfo =
                    Maybe.map (updateNewPersonInfo np)
                        mdl.newPersonInfo
            }

        UpdateWantsPhotos b ->
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


streetForm : String -> Html AddressMsg
streetForm mdl =
    Input.text
        [ Input.onInput UpdateStreet
        , Input.placeholder "Street"
        , Input.value mdl
        , Input.small
        ]


cityForm : String -> Html AddressMsg
cityForm mdl =
    Input.text
        [ Input.onInput UpdateCity
        , Input.placeholder "City"
        , Input.value mdl
        , Input.small
        ]


stateForm : String -> Html AddressMsg
stateForm mdl =
    Input.text
        [ Input.onInput UpdateState
        , Input.placeholder "State"
        , Input.value mdl
        , Input.small
        ]


zipcodeForm : String -> Html AddressMsg
zipcodeForm mdl =
    Input.text
        [ Input.onInput UpdateZip
        , Input.placeholder "Zip"
        , Input.value mdl
        , Input.small
        ]


addressForm : Data.Address -> Html NewPersonInfoMsg
addressForm mdl =
    Html.map UpdateAddress <|
        Form.row []
            [ Form.col []
                [ Form.row [] [ Form.col [] [ streetForm mdl.street ] ]
                , Form.row []
                    [ Form.col [ Col.xs5 ]
                        [ cityForm mdl.city ]
                    , Form.col [ Col.xs3 ]
                        [ stateForm mdl.state ]
                    , Form.col [ Col.xs4 ]
                        [ zipcodeForm mdl.zipcode
                        ]
                    ]
                ]
            ]


currentChurchForm : Maybe String -> Html NewPersonInfoMsg
currentChurchForm mdl =
    let
        setCurrChurch b =
            if b then
                Just ""
            else
                Nothing

        fromMaybe =
            case mdl of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    Form.row []
        [ Form.col []
            [ Checkbox.custom
                [ Checkbox.onCheck (UpdateCurrentChurch << setCurrChurch)
                , Checkbox.checked fromMaybe
                , Checkbox.id "currentChurchId"
                ]
                "Are you regularly attending a church?"
            ]
        , Form.col []
            [ Input.text
                [ Input.onInput (UpdateCurrentChurch << Just)
                , Input.placeholder "Currently Attending Church"
                , Input.value <| Maybe.withDefault "" mdl
                , Input.disabled (not fromMaybe)
                , Input.small
                ]
            ]
        ]


emailForm : String -> Html NewPersonInfoMsg
emailForm mdl =
    Input.text
        [ Input.onInput UpdateEmail
        , Input.placeholder "Email"
        , Input.value mdl
        , Input.small
        ]


firstNameForm : String -> Html PersonMsg
firstNameForm n =
    Input.text
        [ Input.onInput UpdateFirstName
        , Input.value n
        , Input.placeholder "First Name"
        ]


lastNameForm : String -> Html PersonMsg
lastNameForm n =
    Input.text
        [ Input.onInput UpdateFirstName
        , Input.value n
        , Input.placeholder "First Name"
        ]


newPersonInfoForm : Maybe Data.NewPersonInfo -> Html PersonMsg
newPersonInfoForm m =
    m
        |> Maybe.withDefault initNewPersonInfo
        |> (\mdl ->
                Form.row []
                    [ Form.col []
                        [ addressForm mdl.newAddress
                        , currentChurchForm mdl.newCurrentChurch
                        , emailForm mdl.newEmail
                        ]
                    ]
           )
        |> Html.map UpdateNewPersonInfo


tableCell : List (Html msg) -> Table.Cell msg
tableCell =
    Table.td [ Table.cellAttr <| class "d-inline-block col-5 px-1" ]


tableCellShort : List (Html msg) -> Table.Cell msg
tableCellShort =
    Table.td [ Table.cellAttr <| class "d-inline-block col-2 px-1" ]


view : Persons -> Config msg -> Html msg
view ps config =
    let
        head =
            Table.simpleThead config.header

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
