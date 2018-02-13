module CheckIn exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import BreezeApi as BreezeApi
import Data as Data
import ErrorMsg as Err
import FindPeople as Find
import Html as Html exposing (Html, text)


type Msg
    = CheckInClick
    | CheckInResponse (BreezeApi.Response Data.GroupId)
    | Find Find.Msg


type alias Model =
    { groupId : Maybe Data.GroupId
    , find : Find.Model
    , errors : Err.Errors
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        CheckInClick ->
            ( mdl, BreezeApi.checkIn CheckInResponse mdl.find.waitingCheckIn )

        CheckInResponse r ->
            checkInResponse mdl r

        Find m ->
            Find.withUpdate (\m -> { mdl | find = m }) Find m mdl.find


newError : String -> Model -> Model
newError e mdl =
    { mdl | errors = Err.newError e mdl.errors }


checkInResponse : Model -> BreezeApi.Response Data.GroupId -> ( Model, Cmd Msg )
checkInResponse mdl r =
    BreezeApi.fromResponse r
        |> BreezeApi.fromResult
            (\e -> ( newError e { mdl | groupId = Nothing }, Cmd.none ))
            (\gid -> ( { mdl | groupId = Just gid }, Cmd.none ))


view : Model -> Html Msg
view mdl =
    Find.view Find (checkInButtonView mdl) mdl.find


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
