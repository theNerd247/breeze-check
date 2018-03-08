module FindPeople exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import BreezeApi as BreezeApi
import Data as Data
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
import Platform.Cmd as Cmd


type alias HasCheckIn m =
    { m
        | searchLastName : String
        , foundPeople : List Data.Person
        , waitingCheckIn : List Data.Person
        , personNotFound : Bool
        , groupId : Maybe Data.GroupId
    }


type alias HasFind m =
    BreezeApi.HasBreezeApi (HasCheckIn m)


type Msg
    = UpdateSearchLastName String
    | SearchClick
    | SearchResult (BreezeApi.Msg (List Data.Person))
    | ToggleAttending Data.PersonId
    | CheckInClick
    | CheckInResponse (BreezeApi.Msg Data.GroupId)
    | CancelCheckInClick
    | CancelCheckInResponse (BreezeApi.Msg Bool)



-- UPDATE:


update : Msg -> HasFind m -> ( HasFind m, Cmd Msg )
update msg mdl =
    case msg of
        UpdateSearchLastName s ->
            updateSearchLastName mdl s

        SearchClick ->
            searchClick mdl

        SearchResult r ->
            BreezeApi.update searchResult r mdl

        ToggleAttending pid ->
            toggleAttending mdl pid

        CheckInClick ->
            BreezeApi.checkIn CheckInResponse mdl.waitingCheckIn mdl

        CheckInResponse r ->
            BreezeApi.update checkInResponse r mdl

        CancelCheckInClick ->
            BreezeApi.cancelCheckin CancelCheckInResponse mdl.groupId mdl

        CancelCheckInResponse r ->
            BreezeApi.update cancelCheckinResponse r mdl


updateSearchLastName : HasFind m -> String -> ( HasFind m, Cmd Msg )
updateSearchLastName mdl s =
    ( { mdl | searchLastName = s }, Cmd.none )


searchClick : HasFind m -> ( HasFind m, Cmd Msg )
searchClick mdl =
    BreezeApi.findPeople SearchResult mdl.searchLastName mdl


searchResult : List Data.Person -> HasFind m -> ( HasFind m, Cmd Msg )
searchResult ppl mdl =
    let
        mp ps =
            case ps of
                [] ->
                    { mdl | foundPeople = [], personNotFound = True }

                _ ->
                    { mdl | foundPeople = filtered ps, personNotFound = False }

        filtered =
            List.filter personFilter

        pids =
            List.map .pid mdl.waitingCheckIn

        personFilter p =
            not <| List.member p.pid pids
    in
    ( mp ppl, Cmd.none )


toggleAttending : HasFind m -> Data.PersonId -> ( HasFind m, Cmd Msg )
toggleAttending mdl pid =
    let
        ( chin, fnd ) =
            toggleCheckIn pid ( mdl.waitingCheckIn, mdl.foundPeople )
    in
    ( { mdl | waitingCheckIn = chin, foundPeople = fnd }, Cmd.none )


toggleCheckIn : Data.PersonId -> ( List Data.Person, List Data.Person ) -> ( List Data.Person, List Data.Person )
toggleCheckIn pid ( chkin, found ) =
    let
        personFilter p =
            p.pid == pid

        toggleAttend p =
            { p | checkedIn = not p.checkedIn }

        ( ci, co ) =
            List.partition personFilter chkin

        ( fi, fo ) =
            List.partition personFilter found

        newChkin =
            List.append (List.map toggleAttend fi) co

        newFound =
            List.append (List.map toggleAttend ci) fo
    in
    ( newChkin, newFound )


checkInResponse : Data.GroupId -> HasFind m -> ( HasFind m, Cmd Msg )
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
                    |> InputGroup.view
                ]
            ]
        , Form.row [ Row.centerXs ]
            [ Form.col [ Col.xsAuto ] <|
                if not <| String.isEmpty mdl.searchLastName then
                    [ Button.button
                        [ Button.onClick SearchClick
                        , Button.large
                        , Button.outlinePrimary
                        ]
                        [ Html.i [ class "fas fa-search" ] []
                        , text " Search"
                        ]
                    ]
                else
                    []
            ]
        ]


waitingCheckInView : HasFind m -> Html Msg
waitingCheckInView mdl =
    Data.listPersonView (Just ToggleAttending) mdl.waitingCheckIn


foundPeopleView : HasFind m -> Html Msg
foundPeopleView mdl =
    if mdl.loadingStatus then
        Progress.progress
            [ Progress.value 100
            , Progress.animated
            ]
    else if mdl.personNotFound then
        div []
            [ p [ class "text-center text-danger" ] [ text "No one has the last name of" ]
            , h5 [ class "text-center" ] [ text mdl.searchLastName ]
            ]
    else
        Data.listPersonView (Just ToggleAttending) mdl.foundPeople


checkInButton : Html Msg
checkInButton =
    Button.button
        [ Button.success
        , Button.block
        , Button.onClick CheckInClick
        ]
        [ text "Check In "

        --, Html.i [ class "fas fa-sign-in-alt" ] []
        ]


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
