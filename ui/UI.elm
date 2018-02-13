module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Data as Breeze
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
import Html.Attributes exposing (class, for)
import Html.Events exposing (onClick)
import Http as Http
import Time exposing (..)


type alias Model =
    { errors : Err.Errors
    , groupId : Maybe Int
    }


type alias Config =
    { eventName : String
    , apiBase : String
    , debug : Bool
    }


type Msg
    = UpdateLastName String
    | FoundPeople (Result Http.Error (Result Breeze.BreezeException (List Breeze.Person)))
    | ToggleAttending String
    | ErrorMessage Err.Msg
    | NewPersonSelect
    | CheckIn
    | CheckedInGroupId (Result Http.Error (Result Breeze.BreezeException Int))
    | Deb (Debounce.Msg Msg)
    | CancelCheckIn (Result Http.Error (Result Breeze.BreezeException Bool))
    | ClickCancelCheckin


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update config
        , view = view config
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { foundPeople = []
    , checkedIn = []
    , searchLastName = ""
    , errors = Err.model
    , debounce = Debounce.init
    , findPeopleLoading = False
    , groupId = Nothing
    }


config : Config
config =
    { eventName = "Test Event"
    , apiBase = "" -- "http://10.0.0.100:8080"
    , debug = False
    }


debounceCfg : Debounce.Config Model Msg
debounceCfg =
    Debounce.config
        .debounce
        (\dmdl s -> { dmdl | debounce = s })
        Deb
        (500 * millisecond)


newError : String -> Model -> Model
newError msg mdl =
    { mdl | errors = Err.newError msg mdl.errors }


catchResult : (b -> String) -> (a -> Model -> Model) -> Result b a -> Model -> Model
catchResult g f err =
    case err of
        Err e ->
            newError (g e)

        Ok a ->
            f a


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update cfg msg mdl =
    case msg of
        ErrorMessage emsg ->
            ( { mdl | errors = Err.update emsg mdl.errors }, Cmd.none )

        CancelCheckIn t ->
            ( mdl
                |> (t
                        |> catchResult toString
                            (catchResult .breezeErr
                                revertCheckInGroup
                            )
                   )
            , Cmd.none
            )

        ClickCancelCheckin ->
            ( mdl, cancelCheckin cfg mdl.groupId )



-- TODO: add checkin call


updateFoundPeople : List Breeze.Person -> Model -> Model
updateFoundPeople ps mdl =
    if List.isEmpty ps then
        mdl
            |> (newError <|
                    "I'm sorry there's no one with the last name of: "
                        ++ mdl.searchLastName
               )
    else
        { mdl | foundPeople = ps }


toggleCheckIn : String -> ( List Breeze.Person, List Breeze.Person ) -> ( List Breeze.Person, List Breeze.Person )
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


setGroupId : Int -> Model -> Model
setGroupId gid mdl =
    { mdl | groupId = Just gid }


revertCheckInGroup : Bool -> Model -> Model
revertCheckInGroup b mdl =
    if b then
        { mdl | groupId = Nothing }
    else
        mdl


appendIf : Bool -> List a -> List a -> List a
appendIf e f g =
    if e then
        List.append g f
    else
        g


include : List a -> List a -> List a
include a b =
    List.append b a


view : Config -> Model -> Html Msg
view cfg mdl =
    let
        titleRow =
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ h1 [{- class "display-1" -}] [ text cfg.eventName ]
                    , p [] [ text "Mountain View Church" ]
                    ]
                ]
            ]

        errorRow =
            [ Grid.row [] [ Grid.col [] [ Html.map ErrorMessage <| Err.view mdl.errors ] ] ]

        pageRow =
            case mdl.groupId of
                Nothing ->
                    viewCheckin cfg mdl

                Just gid ->
                    viewCheckedIn cfg gid

        body =
            []
                |> flip List.append titleRow
                |> flip List.append errorRow
                |> flip List.append pageRow
    in
    Grid.container [] body


viewAdmin : Config -> Model -> List (Html Msg)
viewAdmin cfg mdl =
    [ Grid.row []
        [ Grid.col [ Col.xs12 ]
            [ h4 [] [ text "Admin" ]
            ]
        ]
    ]
        |> flip List.append viewCheckin


viewCheckedIn : Config -> Int -> List (Html Msg)
viewCheckedIn cfg gid =
    [ Grid.row []
        [ Grid.col [ Col.xs12 ] [ h4 [] [ text "You're ready!" ] ]
        , Grid.col [ Col.xs12 ] [ p [] [ text "Stop by the check-in center to finish the check-in process" ] ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs12 ] [ h4 [] [ text "Check-in ID" ] ]
        , Grid.col [ Col.xs12 ] [ p [] [ text << toString <| gid ] ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs12 ]
            [ Button.button [ Button.danger, Button.large, Button.onClick ClickCancelCheckin ] [ text "Cancel Check-in" ]
            ]
        ]
    ]


viewCheckin : Config -> Model -> List (Html Msg)
viewCheckin cfg mdl =
    let
        foundRow =
            [ Grid.row []
                [ Grid.col [ Col.xs12 ]
                    [ h4 [] [ text "2. Select Members To Check In" ]
                    , listPeople mdl.foundPeople
                    ]
                ]
            ]

        checkedInRow =
            [ Grid.row []
                [ Grid.col [ Col.xs12 ]
                    [ h4 [] [ text "2. Select Members" ]
                    , if List.isEmpty mdl.checkedIn && (not <| List.isEmpty mdl.foundPeople) then
                        p [] [ text "You haven't selected anyone for check-in" ]
                      else
                        listPeople mdl.checkedIn
                    ]
                ]
            , Grid.row [ Row.centerXs ] [ Grid.col [ Col.xsAuto ] ([] |> appendIf (not <| List.isEmpty mdl.checkedIn) [ br [] [], checkInButton, hr [] [] ]) ]
            , Grid.row []
                [ Grid.col [] <|
                    if mdl.findPeopleLoading then
                        [ text "loading..." ]
                    else
                        [ listPeople mdl.foundPeople, br [] [] ]
                ]
            , Grid.row [ Row.centerXs ] [ Grid.col [ Col.xsAuto ] ([] |> appendIf (not <| List.isEmpty mdl.foundPeople) [ newPersonButton, hr [] [] ]) ]
            ]

        personSearchRow =
            [ Grid.row []
                [ Grid.col []
                    [ h4 [] [ text "1. Find Your Family" ]
                    , personSearch
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    []
                ]
            ]
    in
    personSearchRow ++ checkedInRow


newPersonButton : Html Msg
newPersonButton =
    Button.button
        [ Button.outlineSecondary
        , Button.large
        , Button.onClick NewPersonSelect
        ]
        [ text "I can't find us" ]
