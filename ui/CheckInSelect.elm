module CheckInSelect exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import Data as Data
import Debounce as Debounce
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
import Time exposing (millisecond)


type alias Model =
    { searchLastName : String
    , foundPeople : List Data.Person
    , waitingCheckIn : List Data.Person
    , debounce : Debounce.State
    , findPeopleLoading : Bool
    }


type Msg
    = UpdateSearchLastName String
    | FoundPeople (BreezeApi.Response (List Data.Person))
    | AttendingSelected Data.PersonId
    | NotAttendingSelected Data.PersonId
    | Deb (Debounce.Msg Msg)
    | CheckInClick


model : Model
model =
    { foundPeople = []
    , waitingCheckIn = []
    , searchLastName = ""
    , findPeopleLoading = False
    , debounce = Debounce.init
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
-- VIEW:


view : Model -> Html Msg
view mdl =
    Grid.containerFluid []
        [ searchView
        , waitingCheckinView mdl
        , checkInButtonView mdl
        , foundPeopleView mdl
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


waitingCheckinView : Model -> Html Msg
waitingCheckinView mdl =
    Grid.row []
        [ Grid.col [ Col.xs12 ]
            [ h4 [] [ text "2. Select Members" ]
            , if List.isEmpty mdl.waitingCheckIn && (not <| List.isEmpty mdl.foundPeople) then
                p [] [ text "You haven't selected anyone for check-in" ]
              else
                Data.listPersonView NotAttendingSelected mdl.waitingCheckIn
            ]
        ]


checkInButtonView : Model -> Html Msg
checkInButtonView mdl =
    Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xsAuto ] <|
            if not <| List.isEmpty mdl.waitingCheckIn then
                [ br [] []
                , Button.button
                    [ Button.outlineSuccess
                    , Button.large
                    , Button.onClick CheckInClick
                    ]
                    [ text "3. We're Ready! Let's Go!" ]
                , hr [] []
                ]
            else
                []
        ]


foundPeopleView : Model -> Html Msg
foundPeopleView mdl =
    Grid.row []
        [ Grid.col [] <|
            if mdl.findPeopleLoading then
                [ text "loading..." ]
            else
                [ Data.listPersonView AttendingSelected mdl.foundPeople
                , br [] []
                ]
        ]
