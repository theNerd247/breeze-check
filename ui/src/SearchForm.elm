module SearchForm exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html as Html
    exposing
        ( Html
        , a
        , br
        , button
        , div
        , h2
        , h4
        , h5
        , h6
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class)


searchForm : String -> String -> msg -> (String -> msg) -> Html msg
searchForm placeholder mdl onClick updateSearch =
    Form.form []
        [ Form.row [ Row.centerXs ]
            [ Form.col [ Col.xs12 ]
                [ InputGroup.config
                    (InputGroup.text
                        [ Input.placeholder placeholder
                        , Input.onInput updateSearch
                        , Input.value mdl
                        ]
                    )
                    |> InputGroup.large
                    |> InputGroup.successors
                        [ InputGroup.button
                            [ Button.onClick onClick
                            , Button.large
                            , Button.outlinePrimary
                            ]
                            [ Html.i [ class "fas fa-search" ] []
                            ]
                        ]
                    |> InputGroup.view
                ]
            ]
        ]
