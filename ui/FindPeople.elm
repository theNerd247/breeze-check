module FindPeople exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import BreezeApi as BreezeApi
import Data as Data
import Debounce as Debounce
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
import Platform.Cmd as Cmd
import Time exposing (millisecond)


type alias Model =
    { searchLastName : String
    , foundPeople : List Data.Person
    , waitingCheckIn : List Data.Person
    , debounce : Debounce.State
    , findPeopleLoading : Bool
    , errors : Err.Errors
    }


type Msg
    = UpdateSearchLastName String
    | SearchResult (BreezeApi.Response (List Data.Person))
    | ToggleAttending Data.PersonId
    | Deb (Debounce.Msg Msg)


model : Model
model =
    { foundPeople = []
    , waitingCheckIn = []
    , searchLastName = ""
    , findPeopleLoading = False
    , debounce = Debounce.init
    , errors = Err.model
    }


debounceCfg : Debounce.Config Model Msg
debounceCfg =
    Debounce.config
        .debounce
        (\dmdl s -> { dmdl | debounce = s })
        Deb
        (500 * millisecond)


deb1 : (a -> Msg) -> (a -> Msg)
deb1 =
    Debounce.debounce1 debounceCfg



-- UPDATE:


withUpdate : (Model -> mdl) -> (Msg -> msg) -> Msg -> Model -> ( mdl, Cmd msg )
withUpdate fmdl fmsg msg mdl =
    let
        ( m, c ) =
            update msg mdl
    in
    ( fmdl m, Cmd.map fmsg c )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        UpdateSearchLastName s ->
            updateSearchLastName mdl s

        SearchResult r ->
            searchResult mdl r

        ToggleAttending pid ->
            toggleAttending mdl pid

        Deb deb ->
            Debounce.update debounceCfg deb mdl


updateSearchLastName : Model -> String -> ( Model, Cmd Msg )
updateSearchLastName mdl s =
    let
        updatedMdl f =
            { mdl
                | searchLastName = s
                , findPeopleLoading = f
                , foundPeople = []
            }
    in
    if String.isEmpty s then
        ( updatedMdl False, Cmd.none )
    else
        ( updatedMdl True, BreezeApi.findPeople SearchResult s )


newError : String -> Model -> Model
newError e mdl =
    { mdl | errors = Err.newError e mdl.errors }


searchResult : Model -> BreezeApi.Response (List Data.Person) -> ( Model, Cmd Msg )
searchResult mdl r =
    let
        m =
            { mdl | findPeopleLoading = False }
    in
    BreezeApi.fromResponse r
        |> BreezeApi.fromResult
            (\e -> ( newError e m, Cmd.none ))
            (\ppl -> ( { m | foundPeople = ppl }, Cmd.none ))


toggleAttending : Model -> Data.PersonId -> ( Model, Cmd Msg )
toggleAttending mdl pid =
    let
        ( chin, fnd ) =
            toggleCheckIn pid ( mdl.waitingCheckIn, mdl.foundPeople )
    in
    ( { mdl | waitingCheckIn = chin, foundPeople = fnd }, Cmd.none )


toggleCheckIn : String -> ( List Data.Person, List Data.Person ) -> ( List Data.Person, List Data.Person )
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


view : (Msg -> msg) -> Html msg -> Model -> Html msg
view fmsg checkInButtonView mdl =
    Grid.containerFluid []
        [ Html.map fmsg searchView
        , Html.map fmsg <| foundPeopleView mdl
        , Html.map fmsg <| waitingCheckInView mdl
        , checkInButtonView
        ]


searchView : Html Msg
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
                                , Input.onInput (deb1 UpdateSearchLastName)
                                ]
                            )
                            |> InputGroup.large
                            |> InputGroup.view
                        ]
                    ]
                ]
            ]
        ]


waitingCheckInView : Model -> Html Msg
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


foundPeopleView : Model -> Html Msg
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
