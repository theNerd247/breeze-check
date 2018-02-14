module Data exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Html as Html
import Html.Attributes exposing (class, for)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias GroupId =
    Int


type alias PersonId =
    String


decodeGroupId : Decoder (Result BreezeException GroupId)
decodeGroupId =
    withBreezeErrDecoder int


type alias Person =
    { pid : String
    , firstName : String
    , lastName : String
    , checkedIn : Bool
    }


decodePerson : Decoder Person
decodePerson =
    decode Person
        |> required "pid" string
        |> required "firstName" string
        |> required "lastName" string
        |> hardcoded False


decodePersons : Decoder (Result BreezeException (List Person))
decodePersons =
    withBreezeErrDecoder <| list decodePerson


type alias BreezeException =
    { breezeErr : String
    }


decodeBreezeException : Decoder BreezeException
decodeBreezeException =
    decode BreezeException
        |> required "breezeErr" string


withBreezeErrDecoder : Decoder a -> Decoder (Result BreezeException a)
withBreezeErrDecoder d =
    oneOf
        [ map Err decodeBreezeException
        , map Ok d
        , fail "Something went wrong when fetching data"
        ]


encodePersonIds : List Person -> Encode.Value
encodePersonIds =
    Encode.list << List.map (Encode.string << .pid)


type alias EventName =
    String


decodeEventName : Decoder EventName
decodeEventName =
    field "event-name" string



-- VIEW


listPersonView : (PersonId -> msg) -> List Person -> Html.Html msg
listPersonView selected =
    List.map (personView selected >> List.singleton >> ListGroup.li []) >> ListGroup.ul


personView : (PersonId -> msg) -> Person -> Html.Html msg
personView selected p =
    let
        icon =
            if p.checkedIn then
                class "far fa-check-square text-success"
            else
                class "far fa-square"
    in
    Html.a [ onClick (selected p.pid) ]
        [ Grid.containerFluid []
            [ Grid.row []
                [ Grid.col [ Col.xs5 ] [ Html.text p.firstName ]
                , Grid.col [ Col.xs5 ] [ Html.text p.lastName ]
                , Grid.col [ Col.xs2, Col.pushXs5 ] [ Html.i [ icon ] [] ]
                ]
            ]
        ]
