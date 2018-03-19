module Pages.EditFamilyInfoPage exposing (..)

import Bootstrap.Button as Button
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import List.Zipper as Zipper
import NewPerson as NewPerson
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.EditFamilyInfo
    , pageTitle = "New Attendees"
    , pageView = view
    , showInNavbar = True
    }


view : Model -> Html Msg
view mdl =
    pageWrapper ""
        [ div [ class "text-center" ]
            [ h4 []
                [ text <|
                    "Please Provide Info for the "
                        ++ Zipper.current mdl.lastNamesIndex.lastNames
                        ++ " family"
                ]
            , Html.map NewPersonMsg <|
                NewPerson.newPersonInfoForm mdl
            ]
        , nav mdl
        ]


nav : Model -> Html Msg
nav mdl =
    div [ class "grow-6 d-flex flex-row justify-content-center w-100" ]
        [ div [ class "grow-auto px-3" ] [ prevButton mdl ]
        , div [ class "grow-auto px-3" ] [ nextButton mdl ]
        ]


nextButton : Model -> Html Msg
nextButton mdl =
    let
        isLast =
            Zipper.current mdl.lastNamesIndex.lastNames == mdl.lastNamesIndex.lastKey
    in
    Button.button
        [ Button.onClick <|
            NewPersonMsg <|
                if isLast then
                    NewPerson.CreateNewAttendees
                else
                    NewPerson.NextNewInfo
        , Button.outlinePrimary
        ]
        [ text <|
            if isLast then
                "Save"
            else
                "Next Family"
        ]


prevButton : Model -> Html Msg
prevButton mdl =
    let
        isFirst =
            Zipper.current mdl.lastNamesIndex.lastNames == mdl.lastNamesIndex.firstKey
    in
    Button.button
        [ Button.onClick <|
            if isFirst then
                RouterMsg <| Router.SetRoute Router.NewPersons
            else
                NewPersonMsg <| NewPerson.PrevNewInfo
        , Button.outlinePrimary
        ]
        [ text <|
            if isFirst then
                "Back"
            else
                "Prev Family"
        ]
