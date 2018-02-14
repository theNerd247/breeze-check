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


type alias HasCheckin m =
    Find.HasFind
        { m
            | groupId : Maybe Data.GroupId
        }



-- UPDATE


update : Msg -> HasCheckin m -> ( HasCheckin m, Cmd Msg )
update msg mdl =
    case msg of
        CheckInClick ->
            ( mdl, BreezeApi.checkIn CheckInResponse mdl.waitingCheckIn )

        CheckInResponse r ->
            checkInResponse mdl r

        CancelCheckInClick ->
            ( mdl, BreezeApi.cancelCheckin CancelCheckInResponse mdl.groupId )

        CancelCheckInResponse r ->
            cancelCheckinResponse mdl r


checkInResponse : HasCheckin m -> BreezeApi.Response Data.GroupId -> ( HasCheckin m, Cmd Msg )
checkInResponse mdl r =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (flip Err.newError { mdl | groupId = Nothing })
                    (\gid -> { mdl | groupId = Just gid })
    in
    ( m, Cmd.none )


cancelCheckinResponse : HasCheckin m -> BreezeApi.Response Bool -> ( HasCheckin m, Cmd Msg )
cancelCheckinResponse mdl r =
    let
        m =
            BreezeApi.fromResponse r
                |> BreezeApi.fromResult
                    (flip Err.newError mdl)
                    (\_ -> { mdl | groupId = Nothing, foundPeople = [], searchLastName = "" })
    in
    ( m, Cmd.none )



-- VIEW


checkedInView : Maybe Data.GroupId -> Html Msg
checkedInView mgid =
    case mgid of
        Just gid ->
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h4 [] [ text "You're Almost Done!" ]
                    , p [] [ text "Please stop by the check-in desk to finish checking in" ]
                    ]
                ]

        Nothing ->
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xsAuto ]
                    [ h4 [] [ text "Uh oh!" ]
                    , p [] [ text "Some how you checked in without checking in...hit the cancel button below" ]
                    ]
                ]


checkInButtonView : HasCheckin m -> Html Msg
checkInButtonView mdl =
    Grid.row []
        [ Grid.col [ Col.xs12 ] <|
            if not <| List.isEmpty mdl.waitingCheckIn then
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
