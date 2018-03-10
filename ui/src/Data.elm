module Data exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias GroupId =
    Int


decodeGroupId : Decoder GroupId
decodeGroupId =
    int


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
    HasFirstName (HasLastName {})


type alias HasName m =
    { m | name : Name }


setName : Name -> HasName n -> HasName n
setName n m =
    { m | name = n }


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


encodeName : HasName m -> Encode.Value
encodeName n =
    Encode.object
        [ ( "first_name", Encode.string n.name.firstName )
        , ( "last_name", Encode.string n.name.lastName )
        ]


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
        , ( "zipcode", Encode.string m.address.zip )
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
    { m | newPersonInfo : Maybe NewPersonInfo }


initNewPersonInfo : NewPersonInfo
initNewPersonInfo =
    { address = initAddress
    , email = ""
    , currentChurch = ""
    }


encodeNewPersonInfo : NewPersonInfo -> Encode.Value
encodeNewPersonInfo np =
    Encode.object
        [ ( "newAddress", encodeAddress np )
        , ( "newCurrentChurch", Encode.string np.currentChurch )
        , ( "newEmail", Encode.string np.email )
        ]


encodeNewPersonInfos : List NewPersonInfo -> Encode.Value
encodeNewPersonInfos =
    Encode.list << List.map encodeNewPersonInfo


type alias NoPicture =
    Bool


type alias HasNoPicture m =
    { m | wantsPhotos : Bool }


decodeNoPicture : Decoder NoPicture
decodeNoPicture =
    bool


type alias Person =
    HasName
        (HasNewPersonInfo
            (HasNoPicture
                { pid : String
                , checkedIn : Bool
                }
            )
        )


type alias HasPerson m =
    { m | person : Person }


initNewPerson : Person
initNewPerson =
    { pid = ""
    , checkedIn = False
    , newPersonInfo = Nothing
    , name = initName
    , wantsPhotos = False
    }


setNewPersonInfo : NewPersonInfo -> Person -> Person
setNewPersonInfo n m =
    { m | newPersonInfo = Just n }


decodePerson : Decoder Person
decodePerson =
    decode
        (\p n nopic ->
            { name = n
            , pid = p
            , checkedIn = False
            , newPersonInfo = Nothing
            , wantsPhotos = nopic
            }
        )
        |> required "pid" string
        |> required "name" decodeName
        |> required "wantsPhotos" decodeNoPicture


decodePersons : Decoder (List Person)
decodePersons =
    list decodePerson


encodePerson : Person -> Encode.Value
encodePerson p =
    Encode.object
        [ ( "name", encodeName p )
        , ( "pid", Encode.string p.pid )
        , ( "checkedIn", Encode.bool p.checkedIn )
        , ( "newPersonInfo"
          , p.newPersonInfo
                |> Maybe.map encodeNewPersonInfo
                |> Maybe.withDefault Encode.null
          )
        , ( "wantsPhotos", Encode.bool p.wantsPhotos )
        ]


encodePersons : List Person -> Encode.Value
encodePersons =
    Encode.list << List.map encodePerson
