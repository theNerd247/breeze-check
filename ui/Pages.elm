module Pages exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Progress as Progress
import ErrorMsg as Err
import Html as Html exposing (Html, div)
import Monocle.Prism exposing (Prism)
import Nested exposing (modifyCmd, modifyMdl)
import Zipper exposing (..)


type PageDir
    = PrevPage
    | NextPage


type alias Page mdl msg =
    { pgView : mdl -> Html msg
    , pgUpdate : msg -> mdl -> ( mdl, Cmd msg )
    , progress : Float
    }



-- given a Prism between message types, assuming the model type is the same
-- across all functions (because we have structural typing) convert a Page type


mkpage msgl p =
    let
        nestedUpdate msg mdl =
            msgl.getOption msg
                |> Maybe.andThen
                    (Just
                        << modifyCmd msgl.reverseGet
                        << flip p.pgUpdate mdl
                    )
                |> Maybe.withDefault ( mdl, Cmd.none )
    in
    { pgView = Html.map msgl.reverseGet << p.pgView
    , pgUpdate = nestedUpdate
    , progress = p.progress
    }


type alias Pages mdl msg =
    Zipper (Page mdl msg)


type alias Model msg mdl =
    { pagesModel : mdl
    , pages : Pages mdl msg
    }


update pgl msg mdl =
    let
        nextPages =
            case pgl.getOption msg of
                Just NextPage ->
                    next mdl.pages

                Just PrevPage ->
                    prev mdl.pages

                _ ->
                    mdl.pages
    in
    modifyMdl (\m -> { mdl | pagesModel = m, pages = nextPages }) <|
        mdl.pages.selected.pgUpdate msg mdl.pagesModel


view pages errMsg mdl =
    Grid.containerFluid []
        [ Grid.row []
            [ Grid.col [ Col.xs12 ]
                [ pageProgressView <| pages.selected
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ Html.map errMsg <| Err.view mdl.errors
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs12 ]
                [ .pgView pages.selected mdl
                ]
            ]
        ]


pageProgressView : Page mdl msg -> Html msg
pageProgressView pg =
    let
        ops =
            List.append [ Progress.height 10 ] <|
                case pg.progress of
                    33 ->
                        [ Progress.value 33 ]

                    66 ->
                        [ Progress.value 66, Progress.info ]

                    100 ->
                        [ Progress.value 100, Progress.success ]

                    _ ->
                        []
    in
    Progress.progress ops
