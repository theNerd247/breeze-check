module FindPeople exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Progress as Progress
import BreezeApi as BreezeApi
import Data as Data
import ErrorMsg as Err
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


type alias HasFind m =
    Err.HasErrors
        { m
            | searchLastName : String
            , foundPeople : List Data.Person
            , waitingCheckIn : List Data.Person
            , findPeopleLoading : Bool
            , personNotFound : Bool
        }


type Msg
    = UpdateSearchLastName String
    | SearchClick
    | SearchResult (BreezeApi.Response (List Data.Person))
    | ToggleAttending Data.PersonId



-- UPDATE:


update : Msg -> HasFind m -> ( HasFind m, Cmd Msg )
update msg mdl =
    case msg of
        UpdateSearchLastName s ->
            updateSearchLastName mdl s

        SearchClick ->
            searchClick mdl

        SearchResult r ->
            searchResult mdl r

        ToggleAttending pid ->
            toggleAttending mdl pid


updateSearchLastName : HasFind m -> String -> ( HasFind m, Cmd Msg )
updateSearchLastName mdl s =
    ( { mdl | searchLastName = s }, Cmd.none )


searchClick : HasFind m -> ( HasFind m, Cmd Msg )
searchClick mdl =
    ( { mdl | findPeopleLoading = True }, BreezeApi.findPeople SearchResult mdl.searchLastName )


searchResult : HasFind m -> BreezeApi.Response (List Data.Person) -> ( HasFind m, Cmd Msg )
searchResult mdl r =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (flip Err.newError mdl)
                    (\ppl -> mp ppl)

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
    ( { m | findPeopleLoading = False }, Cmd.none )


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



-- VIEW:


searchPersonsView : HasFind m -> Html Msg
searchPersonsView mdl =
    Grid.row [ Row.middleXs ]
        [ Grid.col [ Col.xs12 ]
            [ h2 [] [ text "Find Your Family" ]
            , Form.form []
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
            ]
        ]


waitingCheckInView : HasFind m -> Html Msg
waitingCheckInView mdl =
    Data.listPersonView (Just ToggleAttending) mdl.waitingCheckIn


foundPeopleView : HasFind m -> Html Msg
foundPeopleView mdl =
    if mdl.findPeopleLoading then
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
