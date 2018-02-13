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


model : Errors
model =
    []


newError : String -> Errors -> Errors
newError msg msgs =
    let
        newId =
            Maybe.withDefault 0 << Maybe.map (\x -> 1 + x.errorId) <| List.head msgs

        newMessage =
            { errorId = newId, errorMsg = msg }
    in
    newMessage :: msgs


update : Msg -> Errors -> Errors
update msg errs =
    case msg of
        CloseError eid ->
            List.filter (\x -> x.errorId /= eid) errs


view : (Msg -> msg) -> Errors -> Html msg
view fmsg =
    Html.map fmsg
        << div []
        << List.map error


error : Error -> Html Msg
error msg =
    Alert.danger
        [ text msg.errorMsg
        , button [ class "close", onClick (CloseError msg.errorId) ] [ text "x" ]
        ]
