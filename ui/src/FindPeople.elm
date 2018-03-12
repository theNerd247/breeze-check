module FindPeople exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import Data as Data
import Dict as Dict
import Html as Html
    exposing
        ( Html
        , a
        , br
        , button
        , div
        , h2
        , h5
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class)
import Person as Person
import Platform.Cmd as Cmd


type alias HasCheckIn m =
    { m
        | searchLastName : String
        , foundPeople : Person.Persons
        , waitingCheckIn : Person.Persons
        , personNotFound : Bool
        , groupId : Maybe Data.CheckInGroupId
    }


type alias HasFind m =
    BreezeApi.HasBreezeApi (HasCheckIn m)


type Msg
    = UpdateSearchLastName String
    | SearchClick
    | SearchResponse (BreezeApi.Msg (List Data.Person))
    | CheckInClick
    | CheckInResponse (BreezeApi.Msg Data.CheckInGroupId)
    | CancelCheckInClick
    | CancelCheckInResponse (BreezeApi.Msg Bool)
    | SelectPersonsMsg Person.PersonsMsg
    | EditWaitingMsg Person.PersonsMsg



-- UPDATE:


afterSearch : Msg -> HasFind m -> HasFind m -> HasFind m
afterSearch msg a b =
    case msg of
        SearchResponse (BreezeApi.Recieved (Ok (Ok _))) ->
            b

        _ ->
            a


afterCheckIn : Msg -> HasFind m -> HasFind m -> HasFind m
afterCheckIn msg a b =
    case msg of
        CheckInResponse (BreezeApi.Recieved (Ok (Ok _))) ->
            b

        _ ->
            a


afterCancel : Msg -> HasFind m -> HasFind m -> HasFind m
afterCancel msg a b =
    case msg of
        CancelCheckInResponse (BreezeApi.Recieved (Ok (Ok _))) ->
            b

        _ ->
            a


update : Msg -> HasFind m -> ( HasFind m, Cmd Msg )
update msg mdl =
    case msg of
        UpdateSearchLastName s ->
            updateSearchLastName mdl s

        SearchClick ->
            searchClick mdl

        SearchResponse r ->
            BreezeApi.update searchResult r mdl

        CheckInClick ->
            BreezeApi.checkIn CheckInResponse (Dict.values mdl.waitingCheckIn) mdl

        CheckInResponse r ->
            BreezeApi.update checkInResponse r mdl

        CancelCheckInClick ->
            BreezeApi.cancelCheckin CancelCheckInResponse mdl.groupId mdl

        CancelCheckInResponse r ->
            BreezeApi.update cancelCheckinResponse r mdl

        SelectPersonsMsg msg ->
            let
                updated =
                    Person.updatePersons msg mdl.foundPeople

                newWaiting =
                    Dict.filter
                        (\_ p -> p.checkedIn == Data.SelectedForCheckIn)
                        updated
            in
            ( { mdl
                | foundPeople = updated
                , waitingCheckIn = newWaiting
              }
            , Cmd.none
            )

        EditWaitingMsg msg ->
            ( { mdl | waitingCheckIn = Person.updatePersons msg mdl.waitingCheckIn }, Cmd.none )


updateSearchLastName : HasFind m -> String -> ( HasFind m, Cmd Msg )
updateSearchLastName mdl s =
    ( { mdl | searchLastName = s }, Cmd.none )


searchClick : HasFind m -> ( HasFind m, Cmd Msg )
searchClick mdl =
    BreezeApi.findPeople SearchResponse mdl.searchLastName mdl


searchResult : List Data.Person -> HasFind m -> ( HasFind m, Cmd Msg )
searchResult ppl mdl =
    let
        insertFound waiting ps =
            List.map (\p -> ( p.pid, p )) ps
                |> Dict.fromList
                |> Dict.union waiting

        m =
            case ppl of
                [] ->
                    { mdl | foundPeople = Dict.empty, personNotFound = True }

                _ ->
                    { mdl | foundPeople = insertFound mdl.waitingCheckIn ppl, personNotFound = False }
    in
    ( m, Cmd.none )


checkInResponse : Data.CheckInGroupId -> HasFind m -> ( HasFind m, Cmd Msg )
checkInResponse gid mdl =
    ( { mdl | groupId = Just gid }, Cmd.none )


cancelCheckinResponse : Bool -> HasFind m -> ( HasFind m, Cmd Msg )
cancelCheckinResponse _ mdl =
    ( { mdl | groupId = Nothing }, Cmd.none )



-- VIEW:


searchPersonsForm : HasFind m -> Html Msg
searchPersonsForm mdl =
    Form.form []
        [ Form.row [ Row.centerXs ]
            [ Form.col [ Col.xs12 ]
                [ InputGroup.config
                    (InputGroup.text
                        [ Input.placeholder "Last Name"
                        , Input.onInput UpdateSearchLastName
                        , Input.value mdl.searchLastName
                        ]
                    )
                    |> InputGroup.large
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.onClick SearchClick
                            , Button.large
                            , Button.outlinePrimary
                            ]
                            [ Html.i [ class "fas fa-search" ] []

                            --, text " Search"
                            ]
                        ]
                    |> InputGroup.view
                ]
            ]
        ]


searchResultsView : HasFind m -> Html Msg
searchResultsView mdl =
    if mdl.personNotFound then
        div []
            [ p [ class "text-center text-danger" ] [ text "No one has the last name of" ]
            , h5 [ class "text-center" ] [ text mdl.searchLastName ]
            ]
    else
        Person.selectPersonsForCheckIn
            |> Person.view mdl.foundPeople
            |> Html.map SelectPersonsMsg


cancelCheckInButton : Html Msg
cancelCheckInButton =
    Button.button
        [ Button.outlineInfo
        , Button.large
        , Button.onClick
            CancelCheckInClick
        ]
        [ Html.i [ class "fas fa-arrow-left" ] []
        , text " Cancel Check In"
        ]
