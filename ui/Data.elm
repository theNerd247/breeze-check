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


decodeGroupId : Decoder (Result BreezeException GroupId)
decodeGroupId =
    withBreezeErrDecoder int


type alias PersonId =
    String


type alias FirstName =
    String


type alias HasFirstName m =
    { m | firstName : FirstName }


initFirstName : HasFirstName {}
initFirstName =
    { firstName = "" }


copyFirstName : HasFirstName m -> HasFirstName n -> HasFirstName n
copyFirstName m n =
    { n | firstName = m.firstName }


type alias LastName =
    String


type alias HasLastName m =
    { m | lastName : LastName }


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


type alias NewPersonInfo =
    HasAddress (HasCurrentChurch (HasEmail {}))


type alias HasNewPersonInfo m =
    { m | newPersonInfo : NewPersonInfo }


initNewPersonInfo : NewPersonInfo
initNewPersonInfo =
    { address = initAddress
    , email = ""
    , currentChurch = ""
    }


encodeNewPersonInfo : NewPersonInfo -> Encode.Value
encodeNewPersonInfo np =
    Encode.object
        [ ( "address", encodeAddress np )
        , ( "currentChurch", Encode.string np.currentChurch )
        , ( "email", Encode.string np.email )
        ]


encodeNewPersonInfos : List NewPersonInfo -> Encode.Value
encodeNewPersonInfos =
    Encode.list << List.map encodeNewPersonInfo


type alias Person =
    HasName
        { pid : String
        , checkedIn : Bool
        , newPersonInfo : Maybe NewPersonInfo
        }


type alias HasPerson m =
    { m | person : Person }


initNewPerson : Person
initNewPerson =
    { pid = ""
    , checkedIn = False
    , firstName = ""
    , lastName = ""
    , newPersonInfo = Nothing
    }


setNewPersonInfo : NewPersonInfo -> Person -> Person
setNewPersonInfo n m =
    { m | newPersonInfo = Just n }


decodePerson : Decoder Person
decodePerson =
    decode
        (\p n ->
            { lastName = n.lastName
            , firstName = n.firstName
            , pid = p
            , checkedIn = False
            , newPersonInfo = Nothing
            }
        )
        |> required "pid" string
        |> required "personName" decodeName


decodePersons : Decoder (Result BreezeException (List Person))
decodePersons =
    withBreezeErrDecoder <| list decodePerson


encodePerson : Person -> Encode.Value
encodePerson p =
    Encode.object
        [ ( "firstName", Encode.string p.firstName )
        , ( "lastName", Encode.string p.lastName )
        , ( "pid", Encode.string p.pid )
        , ( "checkedIn", Encode.bool p.checkedIn )
        , ( "newPersonInfo"
          , p.newPersonInfo
                |> Maybe.map encodeNewPersonInfo
                |> Maybe.withDefault Encode.null
          )
        ]


encodePersons : List Person -> Encode.Value
encodePersons =
    Encode.list << List.map encodePerson



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
