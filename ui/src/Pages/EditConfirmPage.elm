module Pages.EditConfirmPage exposing (..)

import Bootstrap.Button as Button
import Dict as Dict
import Html as Html exposing (Html, div, h1, h2, h3, h4, p, text)
import Html.Attributes exposing (class, for, style)
import List.Zipper as Zipper
import NewPerson as NewPerson
import Pages exposing (..)
import Router as Router


config : Config
config =
    { pageRoute = Router.EditConfirm
    , pageTitle = "New Attendees"
    , pageView = view
    , showInNavbar = False
    }


view : Model -> Html Msg
view mdl =
    pageWrapper "text-center"
        [ h4 [] [ text "Doesn't Everything Look Ok?" ]
        , showNewInfo mdl
        , nav mdl
        ]


showNewInfo : Model -> Html Msg
showNewInfo mdl =
    div [ class "d-flex flex-column w-100 justify-content-center align-items-center" ]
        [ div [ class "grow-1 text-" ] [ showNewPeople mdl ]
        , div [ class "grow-auto" ] [ showNewPersonInfos mdl ]
        ]


showNewPeople : Model -> Html Msg
showNewPeople mdl =
    Html.map NewPersonMsg <| NewPerson.newPersonsForm mdl


showNewPersonInfos : Model -> Html Msg
showNewPersonInfos mdl =
    let
        title l =
            "Contact Information For The "
                ++ NewPerson.formatName l
                ++ " Family"

        formWrapper ( l, f ) =
            div [ class "grow-auto text-center" ]
                [ h4 [] [ text <| title l ]
                , NewPerson.npiForm l f
                ]
    in
    mdl.newPersonInfos
        |> Dict.toList
        |> List.map formWrapper
        |> div []
        |> Html.map NewPersonMsg


nav : Model -> Html Msg
nav mdl =
    navButtonsWrapper
        (prevButton mdl)
        (nextButton mdl)


nextButton : Model -> Html Msg
nextButton mdl =
    let
        isLast =
            Zipper.current mdl.lastNamesIndex.lastNames == mdl.lastNamesIndex.lastKey
    in
    Button.button
        [ Button.onClick <| NewPersonMsg <| NewPerson.CreateNewAttendees
        , Button.outlinePrimary
        ]
        [ text "Looks Good!"
        ]


prevButton : Model -> Html Msg
prevButton mdl =
    let
        isFirst =
            Zipper.current mdl.lastNamesIndex.lastNames == mdl.lastNamesIndex.firstKey
    in
    Button.button
        [ Button.onClick <|
            RouterMsg <|
                Router.SetRoute Router.NewPersons
        , Button.outlinePrimary
        ]
        [ text "Go Back"
        ]
