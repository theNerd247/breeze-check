module ErrorMsg exposing (..)

import Bootstrap.Alert as Alert
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , header
        , hr
        , main_
        , p
        , program
        , text
        )
import Html.Attributes exposing (class, for)
import Html.Events exposing (onClick)


type alias Error =
    { errorMsg : String
    , errorId : Int
    }


type alias Errors =
    List Error


type Msg
    = CloseError Int


type alias HasErrors m =
    { m | errors : Errors }


newError : String -> HasErrors m -> HasErrors m
newError msg msgs =
    let
        newId =
            Maybe.withDefault 0 << Maybe.map (\x -> 1 + x.errorId) <| List.head msgs.errors

        newMessage =
            { errorId = newId, errorMsg = msg }
    in
    { msgs | errors = newMessage :: msgs.errors }


update : Msg -> HasErrors m -> HasErrors m
update msg errs =
    case msg of
        CloseError eid ->
            { errs | errors = List.filter (\x -> x.errorId /= eid) errs.errors }


view : HasErrors m -> Html Msg
view =
    div [] << List.map errorView << .errors


errorView : Error -> Html Msg
errorView msg =
    Alert.simpleInfo
        []
        [ text msg.errorMsg
        , button [ class "close", onClick (CloseError msg.errorId) ] [ text "x" ]
        ]
