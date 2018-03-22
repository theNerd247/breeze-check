module FindPeople exposing (..)

import Bootstrap.Button as Button
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
        , h4
        , h5
        , h6
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
import SearchForm as SearchForm


type alias HasCheckIn m =
    { m
        | searchLastName : String
        , foundPeople : Person.Persons
        , waitingCheckIn : Person.Persons
        , personNotFound : Maybe String
        , groupId : Maybe Data.CheckInGroupId
    }


type alias HasFind m =
    BreezeApi.HasBreezeApi (HasCheckIn m)


type Msg
    = LastNamSearch String
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
afterSearch msg b a =
    case msg of
        SearchClick ->
            b

        _ ->
            a


afterCheckIn : Msg -> HasFind m -> HasFind m -> HasFind m
afterCheckIn msg b a =
    case msg of
        CheckInClick ->
            b

        _ ->
            a


afterCancel : Msg -> HasFind m -> HasFind m -> HasFind m
afterCancel msg b a =
    case msg of
        CancelCheckInClick ->
            b

        _ ->
            a


resetFind : HasFind m -> HasFind m
resetFind mdl =
    { mdl
        | searchLastName = ""
        , foundPeople = Person.initPersons
        , personNotFound = Nothing
        , groupId = Nothing
    }


update : Msg -> HasFind m -> ( HasFind m, Cmd Msg )
update msg mdl =
    case msg of
        LastNamSearch s ->
            ( { mdl | searchLastName = s }, Cmd.none )

        SearchClick ->
            BreezeApi.findPeople SearchResponse mdl.searchLastName mdl

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

                updateSel pid m =
                    case m of
                        -- ...specifically the checkedIn field of a
                        -- person
                        -- and we're selecting them for checkin then
                        Person.UpdateCheckedIn Data.SelectedForCheckIn ->
                            Person.updatePersons
                                (mdl.foundPeople
                                    |> Dict.get pid
                                    |> Maybe.withDefault Person.initPerson
                                    |> Person.Create pid
                                )

                        _ ->
                            Person.updatePersons (Person.Delete pid)

                newWaiting =
                    case msg of
                        Person.UpdateAll m ->
                            mdl.foundPeople
                                |> Dict.keys
                                |> List.foldl (\pid f -> f << updateSel pid m) identity

                        -- If we're getting an update on the search result list
                        Person.Update pid m ->
                            updateSel pid m

                        _ ->
                            identity
            in
            ( { mdl
                | foundPeople = updated
                , waitingCheckIn = newWaiting mdl.waitingCheckIn
              }
            , Cmd.none
            )

        EditWaitingMsg msg ->
            ( { mdl | waitingCheckIn = Person.updatePersons msg mdl.waitingCheckIn }, Cmd.none )


searchResult : List Data.Person -> HasFind m -> ( HasFind m, Cmd Msg )
searchResult ppl mdl =
    let
        insertFound =
            List.map (\p -> ( p.pid, { p | wantsPhotos = True } )) ppl
                |> Dict.fromList

        m =
            case ppl of
                [] ->
                    { mdl
                        | foundPeople = insertFound
                        , personNotFound =
                            Just
                                mdl.searchLastName
                    }

                _ ->
                    { mdl
                        | foundPeople = insertFound
                        , personNotFound = Nothing
                    }
    in
    ( m, Cmd.none )


checkInResponse : Data.CheckInGroupId -> HasFind m -> ( HasFind m, Cmd Msg )
checkInResponse gid mdl =
    ( { mdl | groupId = Just gid }, Cmd.none )


cancelCheckinResponse : Bool -> HasFind m -> ( HasFind m, Cmd Msg )
cancelCheckinResponse _ mdl =
    ( { mdl | groupId = Nothing }, Cmd.none )



-- VIEW:


searchResultsView : HasFind m -> Html Msg
searchResultsView mdl =
    let
        nfmsg name =
            div [ class "text-center" ]
                [ h5 [] [ text "No One With  The Last Name Of" ]
                , h6 [] [ text name ]
                , h5 [] [ text "Is Available" ]
                ]

        table =
            Person.selectPersonsForCheckIn
                |> Person.view mdl.foundPeople
                |> Html.map SelectPersonsMsg
    in
    case mdl.personNotFound of
        Just name ->
            nfmsg name

        _ ->
            table


searchPersonsForm : HasFind m -> Html Msg
searchPersonsForm mdl =
    SearchForm.searchForm
        "Last Name"
        mdl.searchLastName
        SearchClick
        LastNamSearch


waitingPersons : HasFind m -> Html Msg
waitingPersons mdl =
    let
        deleteButton p =
            Button.button
                [ Button.onClick <| EditWaitingMsg <| Person.Delete p.pid
                , Button.danger
                , Button.roleLink
                ]
                [ Html.i [ class "far fa-trash-alt text-danger" ] [] ]
    in
    Person.config
        |> Person.cols
            [ deleteButton
            , \p -> text p.personName.firstName
            , \p -> text p.personName.lastName
            ]
        |> Person.view mdl.waitingCheckIn


waitingPersonsWithPhotoSelect : HasFind m -> Html Msg
waitingPersonsWithPhotoSelect mdl =
    Html.map EditWaitingMsg <|
        Person.view
            mdl.waitingCheckIn
            Person.selectPersonsForWantsPhotos


cancelCheckInButton : Html Msg
cancelCheckInButton =
    Button.button
        [ Button.outlineInfo
        , Button.large
        , Button.onClick CancelCheckInClick
        ]
        [ Html.i [ class "fas fa-arrow-left" ] []
        , text " Cancel Check In"
        ]
