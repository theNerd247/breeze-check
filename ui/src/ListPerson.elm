module ListPerson exposing (..)

import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Data as Data
import Html as Html exposing (Html)
import Html.Events exposing (onClick)


type alias Config msg =
    { extras : Maybe (Data.Person -> Html msg)
    , withClick : Maybe (Data.PersonId -> msg)
    }



--type ArrayMsg m a
--= Create a
--| ReplaceAt Int a
--| UpdateAt Int m
--| DeleteAt Int
--updateArray : a -> (m -> a -> a) -> ArrayMsg m a -> List a -> List a
--updateArray default f msg mdl =
--case msg of
--Create x ->
--List.append mdl [ x ]
--ReplaceAt ix x ->
--List.indexedMap
--(\i a ->
--if i == ix then
--x
--else
--a
--)
--mdl
--UpdateAt ix m ->
--List.indexedMap
--(\i a ->
--if i == ix then
--f m a
--else
--a
--)
--mdl
--DeleteAt ix ->
--deleteAt default ix mdl
--deleteAt : Int -> List a -> List a
--deleteAt ix xs =
--let
--pre = List.take ix


config : Config msg
config =
    { extras = Nothing
    , withClick = Nothing
    }


extras : (Data.Person -> Html msg) -> Config msg -> Config msg
extras f mdl =
    { mdl | extras = Just f }


withClick : (Data.PersonId -> msg) -> Config msg -> Config msg
withClick f mdl =
    { mdl | withClick = Just f }



--checkedInIcon : (Data.Person -> Bool) -> (Bool -> msg) -> Data.Person -> Html msg
--checkedInIcon f onc p =
--Checkbox.checkbox
--[ Checkbox.checked <| f p
--, Checkbox.onCheck onc
--]


checkedInIcon : Data.Person -> Html msg
checkedInIcon p =
    Checkbox.checkbox [] "Foo"


onlyListPersons : List Data.Person -> Html msg
onlyListPersons =
    flip view config


view : List Data.Person -> Config msg -> Html.Html msg
view ps config =
    List.map (personView config >> List.singleton >> ListGroup.li []) ps |> ListGroup.ul


personView : Config msg -> Data.Person -> Html.Html msg
personView cfg p =
    let
        extra =
            case cfg.extras of
                Nothing ->
                    []

                Just f ->
                    [ Grid.col [ Col.xs2, Col.pushXs5 ] [ f p ] ]

        withClick x =
            case cfg.withClick of
                Nothing ->
                    x

                Just f ->
                    Html.a [ onClick (f p.pid) ] [ x ]
    in
    withClick <|
        Grid.containerFluid
            []
            [ [ Grid.col [ Col.xs5 ] [ Html.text p.name.firstName ]
              , Grid.col [ Col.xs5 ] [ Html.text p.name.lastName ]
              ]
                |> flip List.append extra
                |> Grid.row []
            ]
