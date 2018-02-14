module CheckIn exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import BreezeApi as BreezeApi
import Data as Data
import ErrorMsg as Err
import FindPeople as Find
import Html as Html exposing (Html, h2, p, text)
import Html.Attributes exposing (class)


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
                    (\_ -> { mdl | groupId = Nothing })
    in
    ( m, Cmd.none )



-- VIEW


checkedInView : Maybe Data.GroupId -> Html Msg
checkedInView mgid =
    case mgid of
        Just gid ->
            Grid.containerFluid []
                [ Grid.row [ Row.centerXs ]
                    [ Grid.col [ Col.xsAuto ]
                        [ h2 [] [ text "You're Almost Done!" ]
                        ]
                    ]
                , Grid.row [ Row.centerXs ]
                    [ Grid.col [ Col.xs10 ]
                        [ p [ class "text-center" ] [ text "Stop by the check-in desk to finish checking in" ]
                        ]
                    ]
                ]

        Nothing ->
            Grid.row [ Row.centerXs ]
                [ Grid.col [ Col.xs10 ]
                    [ h2 [] [ text "Uh oh!" ]
                    , p []
                        [ text "Some how you checked in without checking in...hit the cancel button below"
                        ]
                    ]
                ]


checkInButtonView : Html Msg
checkInButtonView =
    Button.button
        [ Button.outlineSuccess
        , Button.large
        , Button.onClick CheckInClick
        ]
        [ text "Check In "
        , Html.i [ class "fas fa-arrow-right" ] []
        ]


cancelCheckInView : Html Msg
cancelCheckInView =
    Grid.row [ Row.centerXs ]
        [ Grid.col [ Col.xsAuto ]
            [ Button.button
                [ Button.outlineInfo
                , Button.large
                , Button.onClick
                    CancelCheckInClick
                ]
                [ Html.i [ class "fas fa-arrow-left" ] []
                , text " Cancel Check In"
                ]
            ]
        ]
