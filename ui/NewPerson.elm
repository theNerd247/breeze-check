module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Data exposing (..)
import ErrorMsg as Err
import Html as Html exposing (Html, text)
import Html.Attributes exposing (class)
import List.Nonempty as NE exposing (Nonempty(..))


type alias LastName =
    String


type alias FirstName =
    String


type alias NewFamily =
    HasLastName
        (HasAddress
            (HasEmail
                (HasCurrentChurch
                    { memberNames : Nonempty FirstName
                    }
                )
            )
        )


type alias HasNewFamilies m =
    Err.HasErrors { m | newFamilies : Nonempty NewFamily }


type Msg
    = NewPersonsClicked
    | UpdateForm FormMsg


type ArrayMsg m a
    = Create a
    | ReplaceAt Int a
    | UpdateAt Int m
    | DeleteAt Int


type alias FormMsg =
    ArrayMsg FamilyMsg NewFamily


type FamilyMsg
    = UpdateLastName String
    | UpdateAddress AddressMsg
    | UpdateEmail String
    | UpdateCurrentChurch String
    | UpdateMembers (ArrayMsg () String)


type AddressMsg
    = UpdateStreet String
    | UpdateCity String
    | UpdateState String
    | UpdateZip String


initModel : Nonempty NewFamily
initModel =
    NE.fromElement initNewFamily


initNewFamily : NewFamily
initNewFamily =
    { lastName = ""
    , memberNames = NE.fromElement ""
    , currentChurch = ""
    , address = initAddress
    , email = ""
    }


newFamilyToPersons : NewFamily -> NE.Nonempty Person
newFamilyToPersons family =
    let
        newPerson n =
            initNewPerson
                |> copyFirstName { firstName = n }
                |> copyLastName family
                |> setNewPersonInfo
                    (initNewPersonInfo
                        |> copyEmail family
                        |> copyAddress family
                        |> copyCurrentChurch family
                    )
    in
    NE.map newPerson family.memberNames



-- Update


update : (HasNewFamilies m -> List Person -> HasNewFamilies m) -> Msg -> HasNewFamilies m -> ( HasNewFamilies m, Cmd Msg )
update f msg mdl =
    case msg of
        UpdateForm msg ->
            formUpdate msg mdl

        NewPersonsClicked ->
            ( mdl.newFamilies
                |> NE.concatMap newFamilyToPersons
                |> NE.toList
                |> f mdl
            , Cmd.none
            )


formUpdate : FormMsg -> HasNewFamilies m -> ( HasNewFamilies m, Cmd Msg )
formUpdate msg mdl =
    ( { mdl | newFamilies = updateArray initNewFamily newFamilyUpdate msg mdl.newFamilies }, Cmd.none )


newFamilyUpdate : FamilyMsg -> NewFamily -> NewFamily
newFamilyUpdate msg mdl =
    case msg of
        UpdateLastName l ->
            { mdl | lastName = l }

        UpdateMembers msg ->
            { mdl | memberNames = updateArray "" (flip always) msg mdl.memberNames }

        UpdateAddress m ->
            { mdl | address = updateAddress m mdl.address }

        UpdateEmail e ->
            { mdl | email = e }

        UpdateCurrentChurch c ->
            { mdl | currentChurch = c }


updateAddress : AddressMsg -> Address -> Address
updateAddress msg mdl =
    case msg of
        UpdateStreet s ->
            { mdl | street = s }

        UpdateCity c ->
            { mdl | city = c }

        UpdateState s ->
            { mdl | state = s }

        UpdateZip z ->
            { mdl | zip = z }


updateArray : a -> (m -> a -> a) -> ArrayMsg m a -> Nonempty a -> Nonempty a
updateArray default f msg mdl =
    case msg of
        Create x ->
            NE.append mdl <| NE.fromElement x

        ReplaceAt ix x ->
            NE.indexedMap
                (\i a ->
                    if i == ix then
                        x
                    else
                        a
                )
                mdl

        UpdateAt ix m ->
            NE.indexedMap
                (\i a ->
                    if i == ix then
                        f m a
                    else
                        a
                )
                mdl

        DeleteAt ix ->
            deleteAt default ix mdl


deleteAt : a -> Int -> Nonempty a -> Nonempty a
deleteAt x ix ne =
    if ix == 0 then
        if NE.length ne == 1 then
            Nonempty x []
        else
            NE.pop ne
    else
        let
            xs =
                NE.tail ne
        in
        Nonempty (NE.head ne) (List.append (List.take (ix - 1) xs) (List.drop ix xs))


newFamilysView : HasNewFamilies m -> Html Msg
newFamilysView mdl =
    mdl.newFamilies
        |> NE.indexedMap newFamilyForm
        |> NE.toList
        |> flip List.append [ Form.row [] [ Form.col [] [ addFamilyButton ] ] ]
        |> List.map (Html.map UpdateForm)
        |> flip List.append [ Form.row [ Row.centerXs ] [ Form.col [ Col.xsAuto ] [ newFamiliesSubmitButton ] ] ]
        |> Form.form []


newFamilyForm : Int -> NewFamily -> Html FormMsg
newFamilyForm fix nf =
    let
        lastNameForm =
            Form.row []
                [ Form.col [ Col.xs10 ]
                    [ Input.text
                        [ Input.placeholder "Last Name"
                        , Input.value nf.lastName
                        , Input.onInput <| UpdateAt fix << UpdateLastName
                        , Input.small
                        ]
                    ]
                , Form.col [ Col.xs2 ] <|
                    if fix == 0 then
                        []
                    else
                        [ deleteFamilyButton fix ]
                ]

        firstNameForm ix n =
            Form.row []
                [ Form.col [ Col.xs8 ]
                    [ Input.text
                        [ Input.placeholder "Family Member"
                        , Input.value n
                        , Input.onInput <| UpdateAt fix << UpdateMembers << ReplaceAt ix
                        , Input.small
                        ]
                    ]
                , Form.col [ Col.xs2 ] <|
                    if ix == 0 then
                        []
                    else
                        [ Html.map (UpdateAt fix) <| deleteMemberButton ix ]
                , Form.col [ Col.xs2, Col.pullXs2 ] []
                ]

        membersForm =
            NE.indexedMap firstNameForm nf.memberNames
                |> NE.toList
                |> flip List.append
                    [ Form.row []
                        [ Form.col
                            [ Col.xs8 ]
                            [ Html.map (UpdateAt fix) <| addMemberButton ]
                        ]
                    ]

        addressForm =
            [ Form.row []
                [ Form.col [ Col.xs12 ]
                    [ Input.text
                        [ Input.placeholder "Street"
                        , Input.onInput <| UpdateAt fix << UpdateAddress << UpdateStreet
                        ]
                    ]
                ]
            , Form.row []
                [ Form.col [ Col.xs4 ]
                    [ Input.text
                        [ Input.placeholder "City"
                        , Input.onInput <| UpdateAt fix << UpdateAddress << UpdateCity
                        ]
                    ]
                , Form.col [ Col.xs4 ]
                    [ Input.text
                        [ Input.placeholder "State"
                        , Input.onInput <| UpdateAt fix << UpdateAddress << UpdateState
                        ]
                    ]
                , Form.col [ Col.xs4 ]
                    [ Input.text
                        [ Input.placeholder "Zip"
                        , Input.onInput <| UpdateAt fix << UpdateAddress << UpdateZip
                        ]
                    ]
                ]
            ]

        emailForm =
            [ Form.row []
                [ Form.col [ Col.xs12 ]
                    [ Input.text
                        [ Input.placeholder "Email"
                        , Input.onInput <| UpdateAt fix << UpdateEmail
                        ]
                    ]
                ]
            ]

        currentChurchForm =
            [ Form.row []
                [ Form.col [ Col.xs12 ]
                    [ Input.text
                        [ Input.placeholder "Current Church"
                        , Input.onInput <| UpdateAt fix << UpdateCurrentChurch
                        ]
                    ]
                ]
            ]
    in
    [ lastNameForm ]
        |> flip List.append membersForm
        |> flip List.append addressForm
        |> flip List.append emailForm
        |> flip List.append currentChurchForm
        |> Form.group []


addButton : msg -> String -> Html msg
addButton msg t =
    Button.button
        [ Button.onClick msg
        , Button.success
        ]
        [ text t ]


addFamilyButton : Html FormMsg
addFamilyButton =
    addButton (Create initNewFamily) "Add Family"


addMemberButton : Html FamilyMsg
addMemberButton =
    addButton (UpdateMembers <| Create "") "Add Member"


deleteButton : msg -> Html msg
deleteButton msg =
    Button.button
        [ Button.danger
        , Button.onClick msg
        , Button.small
        ]
        [ Html.i [ class "fas fa-times-circle" ] []
        ]


deleteFamilyButton : Int -> Html FormMsg
deleteFamilyButton =
    deleteButton << DeleteAt


deleteMemberButton : Int -> Html FamilyMsg
deleteMemberButton =
    deleteButton << UpdateMembers << DeleteAt


newFamiliesSubmitButton : Html Msg
newFamiliesSubmitButton =
    Button.button
        [ Button.info
        , Button.onClick NewPersonsClicked
        ]
        [ text "Submit" ]
