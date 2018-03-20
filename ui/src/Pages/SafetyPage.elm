module Pages.SafetyPage exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Popover as Popover
import Html as Html exposing (Html, div, h1, h2, h3, h4, h5, p, span, text)
import Html.Attributes exposing (class)
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.Safety
    , pageTitle = "Safety Waiver"
    , pageView = view
    , showInNavbar = False
    }


liabilityStatement : String
liabilityStatement =
    """
  In order to participate with Mountain View Church in the Glow-The- Dark Easter
Egg Hunt on March 23, 2018, I/we on behalf of myself or my/our child participant
release, forever discharge and agree to hold harmless Mountain View Church,
the staff and volunteers thereof from any and all liability, claims or demands for
personal injury, or sickness, as well as property damage and expenses, of
any nature whatsoever which may be incurred by the undersigned and the
participant that occur while said participant is participating in the above described
activity.
  """


nurseNotice : Html msg
nurseNotice =
    div []
        [ p [ class "text-center font-weight-bold" ]
            [ text "Please visit the check-in desk in case of any medical emergencies. We have a nurse on hand"
            ]
        ]


liabilityPopover : String -> Model -> Html Msg
liabilityPopover buttonText mdl =
    let
        popButton t attrs =
            Button.button
                [ Button.roleLink
                , Button.attrs <| Popover.onClick mdl.popoverState PopoverMsg ++ attrs
                ]
                [ text t ]
    in
    Popover.config
        (popButton
            buttonText
            []
        )
        |> Popover.title [ class "text-center" ]
            [ div [ class "float-right" ] [ popButton "X" [ class "close" ] ]
            , h5 [] [ text "Church Event Liability Release Form" ]
            ]
        |> Popover.bottom
        |> Popover.content [ class "text-justify mb-3" ] [ text liabilityStatement ]
        |> Popover.view mdl.popoverState


view : Model -> Html Msg
view mdl =
    pageWrapper ""
        [ div [ class "text-center" ]
            [ h4 [] [ text "Safety First!" ]
            , nurseNotice
            , p []
                [ text "By Selecting \"I Agree\" you are agreeing to Mountain View Church's" ]
            , p []
                [ liabilityPopover "event liability release form" mdl
                ]
            ]
        , div [ class "mb-3" ]
            [ Checkbox.custom
                [ Checkbox.onCheck AgreedToSafetyWaiver
                , Checkbox.checked mdl.aggreedToSafetyWaiver
                , Checkbox.id "safetyAgreement"
                ]
                "I Agree"
            ]
        , navButtonsWrapper
            (backButton Router.Photo)
            (checkInButton
                (not mdl.aggreedToSafetyWaiver)
            )
        ]
