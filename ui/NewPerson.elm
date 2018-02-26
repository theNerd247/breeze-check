module NewPerson exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
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
                    { memberNames : Nonempty String
                    }
                )
            )
        )


type alias HasNewFamilies m =
    Err.HasErrors { m | newFamilies : Nonempty NewFamily }


type Msg
    = NewPersonsClicked
    | NewPersonsResponse (BreezeApi.Response (List Person))
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
    , currentChurch = ""
    , address = initAddress
    , email = ""
    }


newFamilyToNewPersons : NewFamily -> NE.Nonempty NewPerson
newFamilyToNewPersons family =
    let
        newPerson n =
            initNewPerson
                |> copyEmail family
                |> copyAddress family
                |> copyFirstName { firstName = n }
                |> copyLastName family
                |> copyCurrentChurch family
    in
    NE.map newPerson family.memberNames



-- Update


update : (HasNewFamilies m -> List Person -> HasNewFamilies m) -> Msg -> HasNewFamilies m -> ( HasNewFamilies m, Cmd Msg )
update f msg mdl =
    case msg of
        UpdateForm msg ->
            formUpdate msg mdl

        NewPersonsClicked ->
            ( mdl
            , mdl.newFamilies
                |> NE.concatMap newFamilyToNewPersons
                |> NE.toList
                |> flip BreezeApi.newPersons NewPersonsResponse
            )

        NewPersonsResponse r ->
            let
                m =
                    BreezeApi.fromResponse r
                        |> BreezeApi.fromResult
                            (flip Err.newError mdl)
                            (f mdl)
            in
            ( m, Cmd.none )


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
    in
    Form.group [] <| lastNameForm :: membersForm


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
