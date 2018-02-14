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


--type alias mdl =
--{ searchLastName : String
--, foundPeople : List Data.Person
--, waitingCheckIn : List Data.Person
--, findPeopleLoading : Bool
--, errors : Err.Errors
--}


type Msg
    = UpdateSearchLastName String
    | SearchClick
    | SearchResult (BreezeApi.Response (List Data.Person))
    | ToggleAttending Data.PersonId


init m =
    { m
        | foundPeople = []
        , waitingCheckIn = []
        , searchLastName = ""
        , findPeopleLoading = False
        , errors = Err.model
    }



-- UPDATE:


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


updateSearchLastName mdl s =
    ( { mdl | searchLastName = s }, Cmd.none )


searchClick mdl =
    ( mdl, BreezeApi.findPeople SearchResult mdl.searchLastName )


newError e mdl =
    { mdl | errors = Err.newError e mdl.errors }


searchResult mdl r =
    let
        m =
            { mdl | findPeopleLoading = False }
    in
    BreezeApi.fromResponse r
        |> BreezeApi.fromResult
            (\e -> ( newError e m, Cmd.none ))
            (\ppl -> ( { m | foundPeople = ppl }, Cmd.none ))


toggleAttending mdl pid =
    let
        ( chin, fnd ) =
            toggleCheckIn pid ( mdl.waitingCheckIn, mdl.foundPeople )
    in
    ( { mdl | waitingCheckIn = chin, foundPeople = fnd }, Cmd.none )


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


searchPersonsView fmsg =
    Grid.containerFluid []
        [ Html.map fmsg <| searchView
        ]


selectForCheckInView fmsg mdl =
    Grid.containerFluid []
        [ Html.map fmsg <| foundPeopleView mdl
        , Html.map fmsg <| waitingCheckInView mdl
        ]


searchView =
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
                            [ Html.i [ class "far fa-search" ] []
                            , text " Search"
                            ]
                        ]
                    ]
                ]
            ]
        ]


waitingCheckInView mdl =
    Grid.row []
        [ Grid.col [ Col.xs12 ]
            [ h4 [] [ text "2. Select Members" ]
            , if List.isEmpty mdl.waitingCheckIn && (not <| List.isEmpty mdl.foundPeople) then
                p [] [ text "You haven't selected anyone for check-in" ]
              else
                Data.listPersonView ToggleAttending mdl.waitingCheckIn
            ]
        ]


foundPeopleView mdl =
    Grid.row []
        [ Grid.col [] <|
            if mdl.findPeopleLoading then
                [ text "loading..." ]
            else if List.isEmpty mdl.foundPeople then
                [ text <| "No one has the last name of: " ++ mdl.searchLastName ]
            else
                [ Data.listPersonView ToggleAttending mdl.foundPeople
                , br [] []
                ]
        ]
