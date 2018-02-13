module CheckIn exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import Data as Data
import ErrorMsg as Err
import FindPeople as Find
import Html as Html exposing (Html, h4, p, text)


type Msg
    = CheckInClick
    | CheckInResponse (BreezeApi.Response Data.GroupId)
    | CancelCheckInClick
    | CancelCheckInResponse (BreezeApi.Response Bool)
    | Find Find.Msg


type alias Model =
    { groupId : Maybe Data.GroupId
    , find : Find.Model
    , errors : Err.Errors
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        CheckInClick ->
            ( mdl, BreezeApi.checkIn CheckInResponse mdl.find.waitingCheckIn )

        CheckInResponse r ->
            checkInResponse mdl r

        CancelCheckInClick ->
            ( mdl, BreezeApi.cancelCheckin CancelCheckInResponse mdl.groupId )

        CancelCheckInResponse r ->
            cancelCheckinResponse mdl r

        Find m ->
            Find.withUpdate (\m -> { mdl | find = m }) Find m mdl.find


newError : Model -> String -> ( Model, Cmd Msg )
newError mdl e =
    ( { mdl | errors = Err.newError e mdl.errors }, Cmd.none )


checkInResponse : Model -> BreezeApi.Response Data.GroupId -> ( Model, Cmd Msg )
checkInResponse mdl r =
    BreezeApi.fromResponse r
        |> BreezeApi.fromResult
            (newError { mdl | groupId = Nothing })
            (\gid -> ( { mdl | groupId = Just gid }, Cmd.none ))


cancelCheckinResponse : Model -> BreezeApi.Response Bool -> ( Model, Cmd Msg )
cancelCheckinResponse mdl r =
    let
        f m =
            { m | foundPeople = [], searchLastName = "" }
    in
    BreezeApi.fromResponse r
        |> BreezeApi.fromResult
            (newError mdl)
            (\_ -> ( { mdl | groupId = Nothing, find = f mdl.find }, Cmd.none ))



-- VIEW


checkedInView : Data.GroupId -> Html Msg
checkedInView gid =
    Grid.containerFluid []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ h4 [] [ text "You're Almost Done!" ]
                , p [] [ text "Please stop by the check-in desk to finish checking in" ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ cancelCheckInView
                ]
            ]
        ]


checkInButtonView : Model -> Html Msg
checkInButtonView mdl =
    Grid.row []
        [ Grid.col [ Col.xs12 ] <|
            if not <| List.isEmpty mdl.find.waitingCheckIn then
                [ Button.button
                    [ Button.outlineSuccess
                    , Button.large
                    , Button.onClick CheckInClick
                    ]
                    [ text "3. We're Ready! Let's Go!" ]
                ]
            else
                []
        ]


cancelCheckInView : Html Msg
cancelCheckInView =
    Grid.row []
        [ Grid.col [ Col.xs12 ]
            [ Button.button
                [ Button.danger
                , Button.large
                , Button.onClick
                    CancelCheckInClick
                ]
                [ text "Cancel Check-in" ]
            ]
        ]
