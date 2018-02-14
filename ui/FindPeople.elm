module FindPeople exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
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
        , h1
        , h4
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
    ( mdl, BreezeApi.findPeople SearchResult mdl.searchLastName )


searchResult : HasFind m -> BreezeApi.Response (List Data.Person) -> ( HasFind m, Cmd Msg )
searchResult mdl r =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (flip Err.newError { mdl | findPeopleLoading = False })
                    (\ppl -> { mdl | foundPeople = ppl })
    in
    ( m, Cmd.none )


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


selectForCheckInView : HasFind m -> Html Msg
selectForCheckInView mdl =
    let
        title =
            Grid.row [] [ Grid.col [ Col.xs12 ] [ h4 [] [ text "Select Your Family Members" ] ] ]
    in
    Grid.containerFluid []
        [ title
        , foundPeopleView mdl
        , waitingCheckInView mdl
        ]


searchPersonsView : Html Msg
searchPersonsView =
    Grid.row []
        [ Grid.col []
            [ h4 [] [ text "1. Find Your Family" ]
            , Form.form []
                [ Form.row []
                    [ Form.col []
                        [ InputGroup.config
                            (InputGroup.text
                                [ Input.placeholder "Last Name"
                                , Input.onInput UpdateSearchLastName
                                ]
                            )
                            |> InputGroup.large
                            |> InputGroup.view
                        ]
                    ]
                , Form.row []
                    [ Form.col []
                        [ Button.button
                            [ Button.onClick SearchClick, Button.large ]
                            [ Html.i [ class "fas fa-search" ] []
                            , text " Search"
                            ]
                        ]
                    ]
                ]
            ]
        ]


waitingCheckInView : HasFind m -> Html Msg
waitingCheckInView mdl =
    Grid.row []
        [ Grid.col [ Col.xs12 ]
            [ if List.isEmpty mdl.waitingCheckIn && (not <| List.isEmpty mdl.foundPeople) then
                p [] [ text "You haven't selected anyone for check-in" ]
              else
                Data.listPersonView ToggleAttending mdl.waitingCheckIn
            ]
        ]


foundPeopleView : HasFind m -> Html Msg
foundPeopleView mdl =
    Grid.row []
        [ Grid.col [] <|
            if mdl.findPeopleLoading then
                [ text "loading..." ]
            else if
                List.isEmpty mdl.foundPeople
                    && not
                        (String.isEmpty
                            mdl.searchLastName
                        )
            then
                [ text <| "No one has the last name of: " ++ mdl.searchLastName ]
            else
                [ Data.listPersonView ToggleAttending mdl.foundPeople
                , br [] []
                ]
        ]
