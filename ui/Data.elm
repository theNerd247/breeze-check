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


type alias HasFirstName m =
    { m | firstName : String }


initFirstName : HasFirstName {}
initFirstName =
    { firstName = "" }


copyFirstName : HasFirstName m -> HasFirstName n -> HasFirstName n
copyFirstName m n =
    { n | firstName = m.firstName }


type alias HasLastName m =
    { m | lastName : String }


initLastName : HasLastName {}
initLastName =
    { lastName = "" }


copyLastName : HasLastName m -> HasLastName n -> HasLastName n
copyLastName m n =
    { n | lastName = m.lastName }


type alias Name =
    HasName {}


type alias HasName m =
    HasFirstName (HasLastName m)


initName : Name
initName =
    { firstName = initFirstName.firstName
    , lastName = initLastName.lastName
    }


decodeName : Decoder Name
decodeName =
    decode (\f l -> { firstName = f, lastName = l })
        |> required "firstName" string
        |> required "lastName" string


type alias Person =
    HasName
        { pid : String
        , checkedIn : Bool
        }


type alias HasPerson m =
    { m | person : Person }


decodePerson : Decoder Person
decodePerson =
    decode
        (\p n c ->
            { lastName = n.lastName
            , firstName = n.firstName
            , pid = p
            , checkedIn = c
            }
        )
        |> required "pid" string
        |> required "personName" decodeName
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


type alias Address =
    { street : String
    , city : String
    , state : String
    , zip : String
    }


type alias HasAddress m =
    { m | address : Address }


copyAddress : HasAddress m -> HasAddress n -> HasAddress n
copyAddress m n =
    { n | address = m.address }


encodeAddress : HasAddress m -> Encode.Value
encodeAddress m =
    Encode.object
        [ ( "street", Encode.string m.address.street )
        , ( "city", Encode.string m.address.city )
        , ( "state", Encode.string m.address.state )
        , ( "zip", Encode.string m.address.zip )
        ]


initAddress : Address
initAddress =
    { street = ""
    , city = ""
    , state = ""
    , zip = ""
    }


type alias CurrentChurch =
    String


type alias HasCurrentChurch m =
    { m | currentChurch : CurrentChurch }


copyCurrentChurch : HasCurrentChurch m -> HasCurrentChurch n -> HasCurrentChurch n
copyCurrentChurch m n =
    { n | currentChurch = m.currentChurch }


type alias Email =
    String


type alias HasEmail m =
    { m | email : Email }


copyEmail : HasEmail m -> HasEmail n -> HasEmail n
copyEmail m n =
    { n | email = m.email }


type alias NewPerson =
    HasName (HasAddress (HasCurrentChurch (HasEmail {})))


initNewPerson : NewPerson
initNewPerson =
    { firstName = ""
    , lastName = ""
    , address = initAddress
    , email = ""
    , currentChurch = ""
    }


encodeNewPerson : NewPerson -> Encode.Value
encodeNewPerson np =
    Encode.object
        [ ( "newName"
          , Encode.object
                [ ( "last_name", Encode.string np.lastName )
                , ( "first_name", Encode.string np.firstName )
                ]
          )
        , ( "address", encodeAddress np )
        , ( "currentChurch", Encode.string np.currentChurch )
        , ( "email", Encode.string np.email )
        ]


encodeNewPersons : List NewPerson -> Encode.Value
encodeNewPersons =
    Encode.list << List.map encodeNewPerson



-- VIEW


listPersonView : Maybe (PersonId -> msg) -> List Person -> Html.Html msg
listPersonView selected =
    List.map (personView selected >> List.singleton >> ListGroup.li []) >> ListGroup.ul


personView : Maybe (PersonId -> msg) -> Person -> Html.Html msg
personView selected p =
    let
        icon =
            if p.checkedIn then
                [ class "far fa-check-square text-success" ]
            else
                [ class "far fa-square" ]

        checkBox =
            case selected of
                Nothing ->
                    []

                _ ->
                    [ Grid.col [ Col.xs2, Col.pushXs5 ] [ Html.i icon [] ] ]

        withClick x =
            case selected of
                Nothing ->
                    x

                Just f ->
                    Html.a [ onClick (f p.pid) ] [ x ]
    in
    withClick <|
        Grid.containerFluid
            []
            [ [ Grid.col [ Col.xs5 ] [ Html.text p.firstName ]
              , Grid.col [ Col.xs5 ] [ Html.text p.lastName ]
              ]
                |> flip List.append checkBox
                |> Grid.row []
            ]
