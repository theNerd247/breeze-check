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
    | PersonMsg PersonMsg


type PersonMsg
    = UpdateLastName String
    | UpdateFirstName String
    | UpdateName Data.Name
    | UpdateCheckedIn Data.CheckInStatus
    | UpdateNewPersonInfo NewPersonInfoMsg
    | UpdateWantsPhotos Bool
    | UpdateIsParent Bool


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
    { cols : List (Data.Person -> Html msg)
    , rowOptions : Data.Person -> List (Table.RowOption msg)
    , cellOptions : List (Table.CellOption msg)
    , header : List (Html msg)
    , headerOptions : List (Table.CellOption msg)
    , showIfEmpty : Bool
    }


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
    , isParent = False
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

        _ ->
            mdl


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

        UpdateIsParent b ->
            { mdl | isParent = b }


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


config : Config msg
config =
    { cols = []
    , rowOptions = always []
    , cellOptions = []
    , header = []
    , headerOptions = []
    , showIfEmpty = False
    }


cols : List (Data.Person -> Html msg) -> Config msg -> Config msg
cols f mdl =
    { mdl | cols = f }


mapConfig : (a -> b) -> Config a -> Config b
mapConfig f mdl =
    { mdl
        | cols = List.map (\g -> Html.map f << g) mdl.cols
        , rowOptions = always []
        , header = List.map (Html.map f) mdl.header
        , headerOptions = []
        , cellOptions = []
        , showIfEmpty = mdl.showIfEmpty
    }



-- | If you are using map make sure to set this AFTER the map is performed so
-- the resulting typ is mapped correctly. This is due to Table.RowOptions not
-- being a functor


rowOptions : (Data.Person -> List (Table.RowOption msg)) -> Config msg -> Config msg
rowOptions f cfg =
    { cfg | rowOptions = f }


header : List (Html msg) -> Config msg -> Config msg
header f mdl =
    { mdl | header = f }


headerOptions : List (Table.CellOption msg) -> Config msg -> Config msg
headerOptions ops mdl =
    { mdl | headerOptions = ops }


showIfEmpty : Bool -> Config a -> Config a
showIfEmpty x mdl =
    { mdl | showIfEmpty = x }


cellOptions : List (Table.CellOption msg) -> Config msg -> Config msg
cellOptions ops mdl =
    { mdl | cellOptions = ops }


editPersons : Data.Person -> Config PersonsMsg
editPersons newPerson =
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

        editHeader =
            [ addButton
            , Html.map PersonMsg <| firstNameForm newPerson.personName.firstName
            , Html.map PersonMsg <| lastNameForm newPerson.personName.lastName
            ]

        addButton =
            Button.button
                [ Button.onClick <| Create newPerson
                , Button.success
                , Button.disabled <|
                    String.isEmpty
                        newPerson.personName.firstName
                        || String.isEmpty newPerson.personName.lastName
                ]
                [ text "Add" ]
    in
    config
        |> showIfEmpty True
        |> header editHeader
        |> cols
            [ deleteButton
            , setNameView "First Name" UpdateFirstName (.personName >> .firstName)
            , setNameView "Last Name" UpdateLastName (.personName >> .lastName)
            ]


selectPersons : (Bool -> PersonMsg) -> (Data.Person -> Bool) -> Config PersonsMsg
selectPersons f personChecked =
    let
        selheader =
            Checkbox.custom
                [ Checkbox.onCheck <| UpdateAll << f
                , Checkbox.id "selectAll"
                , Checkbox.inline
                ]
                ""

        selPerson p =
            Checkbox.custom
                [ Checkbox.id <| toString p.pid
                , Checkbox.checked <| personChecked p
                , Checkbox.onCheck <| Update p.pid << f
                , Checkbox.inline
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
        |> cols
            [ selPerson
            , \p -> text p.personName.firstName
            , \p -> text p.personName.lastName
            ]
        |> header [ selheader, text "", text "" ]
        |> headerOptions
            [ Table.cellAttr <| class "col-xs-1"
            , Table.cellAttr <| class "col-xs-5"
            , Table.cellAttr <| class "col-xs-5"
            ]
        |> rowOptions setActive


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
    selectPersons m personChecked


selectPersonsForWantsPhotos : Config PersonsMsg
selectPersonsForWantsPhotos =
    selectPersons UpdateWantsPhotos .wantsPhotos


onlyListPersons : Persons -> Html msg
onlyListPersons ps =
    config
        |> cols
            [ \p -> text p.personName.firstName
            , \p -> text p.personName.lastName
            ]
        |> view ps


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
        [ Input.onInput UpdateLastName
        , Input.value n
        , Input.placeholder "Last Name"
        ]


isParentForm : Bool -> Html PersonMsg
isParentForm mdl =
    Checkbox.checkbox
        [ Checkbox.onCheck UpdateIsParent
        , Checkbox.checked mdl
        , Checkbox.id "isParent"
        ]
        "I'm An Adult"


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


makeCells : List (Table.CellOption msg) -> List (Html msg) -> List (Table.Cell msg)
makeCells ops cells =
    let
        size =
            max (List.length ops) (List.length cells)

        emptyOp =
            Table.cellAttr <| class ""

        makeSized x xs =
            List.take size <| xs ++ List.repeat (size - List.length xs) x

        os =
            makeSized emptyOp ops

        cs =
            makeSized (text "") cells
    in
    List.map2 (\op c -> Table.td [ op ] [ c ]) os cs


view : Persons -> Config msg -> Html msg
view ps config =
    let
        hs =
            List.length config.cols - List.length config.header

        head =
            makeCells config.headerOptions config.header
                |> Table.simpleThead

        body =
            Dict.values ps
                |> List.map (personRow config)
                |> Table.tbody []
    in
    if not config.showIfEmpty && Dict.isEmpty ps then
        text ""
    else
        Table.table
            { thead = head
            , tbody = body
            , options = [ Table.hover, Table.striped ]
            }


personRow : Config msg -> Data.Person -> Table.Row msg
personRow config p =
    makeCells config.cellOptions (List.map (\f -> f p) config.cols)
        |> Table.tr
            (config.rowOptions p)
