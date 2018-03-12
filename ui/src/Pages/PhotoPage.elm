module Pages.PhotoPage exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import FindPeople as Find
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Person as Person


type alias Msg =
    Find.Msg


type alias HasPhotoPage m =
    Find.HasFind m


update : Msg -> HasPhotoPage m -> ( HasPhotoPage m, Cmd Msg )
update =
    Find.update


view : HasPhotoPage m -> Html Msg
view mdl =
    div []
        [ Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs12 ]
                [ h1 [] [ text "May We Take You're Picture Please?" ]
                , p []
                    [ text
                        """
              We have a photographer here taking photos for our website and
              other promotional materials here at Mountain View Church. You may
              be in a few of our photos and we would like your permission to
              publish any photo taken that has you or your family in it.
                """
                    ]
                , p []
                    [ Html.b [] [ text "We won't publish any names or contant information!" ]
                    ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ Html.map Find.EditWaitingMsg <|
                    Person.view mdl.waitingCheckIn
                        Person.selectPersonsForWantsPhotos
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xsAuto ]
                [ checkInButton
                ]
            ]
        ]


checkInButton : Html Msg
checkInButton =
    Button.button
        [ Button.success
        , Button.block
        , Button.onClick Find.CheckInClick
        ]
        [ text "Check In "
        ]
