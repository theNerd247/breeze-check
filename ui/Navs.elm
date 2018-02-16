module Navs exposing (..)

import Html as Html exposing (Html, a, div, li, ul)
import Html.Attributes exposing (class, for, style)
import Html.Events exposing (onClick)


view : Int -> List (Bool -> Html msg) -> Html msg
view activeIndex =
    let
        setActive i f =
            f <| i == activeIndex
    in
    nav << List.indexedMap setActive


nav : List (Html msg) -> Html msg
nav items =
    ul [ class "nav nav-pills nav-fill", style [ ( "background-color", "#FFF" ) ] ] items


navItem : Html msg -> msg -> Bool -> Html msg
navItem content onclick active =
    let
        navItemClass =
            if active then
                class "nav-link active"
            else
                class "nav-link"
    in
    li [ class "nav-item" ]
        [ a [ onClick onclick, navItemClass, style [ ( "border-radius", "0" ) ] ] [ content ]
        ]
