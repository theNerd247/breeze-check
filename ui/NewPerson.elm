module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import BreezeApi as BreezeApi
import Data as Data
import Html as Html exposing (Html, text)
import Html.Attributes exposing (class)
import List.Nonempty as NE exposing (Nonempty(..))


type alias LastName =
    String


type alias FirstName =
    String


type alias NewFamily =
    { lastName : String
    , memberNames : Nonempty String
    }


type alias HasNewPersons m =
    { m | newPersons : Nonempty NewFamily }


type Msg
    = NewPersonsClicked
    | NewPersonsResponse (BreezeApi.Response (List Data.Person))
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
    | UpdateMembers (ArrayMsg () String)


initModel : Nonempty NewFamily
initModel =
    NE.fromElement initNewFamily


initNewFamily : NewFamily
initNewFamily =
    { lastName = ""
    , memberNames = NE.fromElement ""
    }


newFamilyToNewPersons : NewFamily -> List Data.NewPerson
newFamilyToNewPersons family =
    let
        newPerson n =
            { lastName = family.lastName
            , firstName = n
            }
    in
    NE.toList <| NE.map newPerson family.memberNames



-- Update


update : Msg -> HasNewPersons m -> ( HasNewPersons m, Cmd Msg )
update msg mdl =
    case msg of
        UpdateForm msg ->
            formUpdate msg mdl

        _ ->
            ( mdl, Cmd.none )


formUpdate : FormMsg -> HasNewPersons m -> ( HasNewPersons m, Cmd Msg )
formUpdate msg mdl =
    ( { mdl | newPersons = updateArray initNewFamily newFamilyUpdate msg mdl.newPersons }, Cmd.none )


newFamilyUpdate : FamilyMsg -> NewFamily -> NewFamily
newFamilyUpdate msg mdl =
    case msg of
        UpdateLastName l ->
            { mdl | lastName = l }

        UpdateMembers msg ->
            { mdl
                | memberNames =
                    updateArray "" (flip always) msg mdl.memberNames
                        |> NE.filter (not << String.isEmpty) ""
            }


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


newFamilysView : HasNewPersons m -> Html Msg
newFamilysView mdl =
    let
        familyForm ix f =
            newFamilyForm f
                |> List.map (Html.map <| UpdateAt ix)
                |> List.append [ Form.row [] [ Form.col [] [ deleteFamilyButton ix ] ] ]
                |> Form.group []
    in
    mdl.newPersons
        |> NE.indexedMap familyForm
        |> NE.toList
        |> flip List.append [ addFamilyInput ]
        |> Form.form []
        |> Html.map UpdateForm


newFamilyForm : NewFamily -> List (Html FamilyMsg)
newFamilyForm nf =
    let
        lastNameForm =
            Form.row []
                [ Form.col []
                    [ Input.text
                        [ Input.placeholder "Last Name"
                        , Input.value nf.lastName
                        , Input.onInput UpdateLastName
                        ]
                    ]
                ]

        firstNameForm ix n =
            Form.row []
                [ Form.col [ Col.xs2 ] [ deleteMemberButton ix ]
                , Form.col [ Col.xs10 ]
                    [ Input.text
                        [ Input.placeholder "Family Member"
                        , Input.value n
                        , Input.onInput <| UpdateMembers << ReplaceAt ix
                        ]
                    ]
                ]

        membersForm =
            NE.indexedMap firstNameForm nf.memberNames
                |> NE.toList
                |> flip List.append
                    [ Form.row []
                        [ Form.col [ Col.xs2 ] [ Html.i [ class "fas fa-user-plus" ] [] ]
                        , Form.col [ Col.xs10 ] [ addMemberInput ]
                        ]
                    ]
    in
    lastNameForm
        :: membersForm


deleteFamilyButton : Int -> Html FormMsg
deleteFamilyButton ix =
    Button.button
        [ Button.danger
        , Button.onClick <| DeleteAt ix

        --, Button.attrs [ class "text-danger" ]
        ]
        [ Html.i [ class "fas fa-minus-circle" ] []

        --, text "Delete"
        ]


addFamilyInput : Html FormMsg
addFamilyInput =
    Input.text
        [ Input.placeholder "New Family Last Name"
        , Input.onInput <| Create << (\l -> { initNewFamily | lastName = l })
        ]


deleteMemberButton : Int -> Html FamilyMsg
deleteMemberButton ix =
    Button.button
        [ Button.danger
        , Button.onClick <| UpdateMembers <| DeleteAt ix

        --, Button.attrs [ class "text-danger" ]
        ]
        [ Html.i [ class "fas fa-minus-circle" ] []

        --, text "Delete"
        ]


addMemberInput : Html FamilyMsg
addMemberInput =
    Input.text
        [ Input.placeholder "New Member First Name"
        , Input.onInput <| UpdateMembers << Create
        ]
