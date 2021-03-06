module Data exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode

-- This file is autogenerated! Please do not edit!

-- Generated on: 2018-03-23 06:29:17.279600234 UTC

type alias CheckInGroupId = Int

type alias ChurchInfo = String

type alias Email = String

type alias EventId = String

type alias FirstName = String

type alias LastName = String

type alias PersonId = Int

type alias Phone = String

type alias TempPersonId = PersonId

type alias BreezeException =
    { breezeErr : String
    }

decodeBreezeException : Decoder BreezeException
decodeBreezeException =
    decode BreezeException
        |> required "breezeErr" string

encodeBreezeException : BreezeException -> Json.Encode.Value
encodeBreezeException x =
    Json.Encode.object
        [ ( "breezeErr", Json.Encode.string x.breezeErr )
        ]

type alias Person =
    { pid : Int
    , personName : Name
    , checkedIn : CheckInStatus
    , newPersonInfo : Maybe (NewPersonInfo)
    , wantsPhotos : Bool
    , isParent : Bool
    }

decodePerson : Decoder Person
decodePerson =
    decode Person
        |> required "pid" int
        |> required "personName" decodeName
        |> required "checkedIn" decodeCheckInStatus
        |> required "newPersonInfo" (nullable decodeNewPersonInfo)
        |> required "wantsPhotos" bool
        |> required "isParent" bool

encodePerson : Person -> Json.Encode.Value
encodePerson x =
    Json.Encode.object
        [ ( "pid", Json.Encode.int x.pid )
        , ( "personName", encodeName x.personName )
        , ( "checkedIn", encodeCheckInStatus x.checkedIn )
        , ( "newPersonInfo", (Maybe.withDefault Json.Encode.null << Maybe.map encodeNewPersonInfo) x.newPersonInfo )
        , ( "wantsPhotos", Json.Encode.bool x.wantsPhotos )
        , ( "isParent", Json.Encode.bool x.isParent )
        ]

type alias Name =
    { firstName : String
    , lastName : String
    }

decodeName : Decoder Name
decodeName =
    decode Name
        |> required "firstName" string
        |> required "lastName" string

encodeName : Name -> Json.Encode.Value
encodeName x =
    Json.Encode.object
        [ ( "firstName", Json.Encode.string x.firstName )
        , ( "lastName", Json.Encode.string x.lastName )
        ]

type CheckInStatus
    = CheckedOut
    | WaitingApproval Int
    | CheckedIn
    | WaitingCreation Int Int
    | SelectedForCheckIn

decodeCheckInStatus : Decoder CheckInStatus
decodeCheckInStatus =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "CheckedOut" ->
                        decode CheckedOut

                    "WaitingApproval" ->
                        decode WaitingApproval
                            |> required "contents" int

                    "CheckedIn" ->
                        decode CheckedIn

                    "WaitingCreation" ->
                        decode WaitingCreation
                            |> required "contents" (index 0 int)
                            |> required "contents" (index 1 int)

                    "SelectedForCheckIn" ->
                        decode SelectedForCheckIn

                    _ ->
                        fail "Constructor not matched"
            )

encodeCheckInStatus : CheckInStatus -> Json.Encode.Value
encodeCheckInStatus x =
    case x of
        CheckedOut ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CheckedOut" )
                , ( "contents", Json.Encode.list [] )
                ]

        WaitingApproval y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "WaitingApproval" )
                , ( "contents", Json.Encode.int y0 )
                ]

        CheckedIn ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CheckedIn" )
                , ( "contents", Json.Encode.list [] )
                ]

        WaitingCreation y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "WaitingCreation" )
                , ( "contents", Json.Encode.list [ Json.Encode.int y0, Json.Encode.int y1 ] )
                ]

        SelectedForCheckIn ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SelectedForCheckIn" )
                , ( "contents", Json.Encode.list [] )
                ]

type alias NewPersonInfo =
    { newAddress : Address
    , newCurrentChurch : Maybe (String)
    , newEmail : String
    , fullyNew : Bool
    }

decodeNewPersonInfo : Decoder NewPersonInfo
decodeNewPersonInfo =
    decode NewPersonInfo
        |> required "newAddress" decodeAddress
        |> required "newCurrentChurch" (nullable string)
        |> required "newEmail" string
        |> required "fullyNew" bool

encodeNewPersonInfo : NewPersonInfo -> Json.Encode.Value
encodeNewPersonInfo x =
    Json.Encode.object
        [ ( "newAddress", encodeAddress x.newAddress )
        , ( "newCurrentChurch", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.newCurrentChurch )
        , ( "newEmail", Json.Encode.string x.newEmail )
        , ( "fullyNew", Json.Encode.bool x.fullyNew )
        ]

type alias Address =
    { street : String
    , city : String
    , state : String
    , zipcode : String
    }

decodeAddress : Decoder Address
decodeAddress =
    decode Address
        |> required "street" string
        |> required "city" string
        |> required "state" string
        |> required "zipcode" string

encodeAddress : Address -> Json.Encode.Value
encodeAddress x =
    Json.Encode.object
        [ ( "street", Json.Encode.string x.street )
        , ( "city", Json.Encode.string x.city )
        , ( "state", Json.Encode.string x.state )
        , ( "zipcode", Json.Encode.string x.zipcode )
        ]

type alias EventInfo =
    { eventId : String
    , eventName : String
    }

decodeEventInfo : Decoder EventInfo
decodeEventInfo =
    decode EventInfo
        |> required "eventId" string
        |> required "eventName" string

encodeEventInfo : EventInfo -> Json.Encode.Value
encodeEventInfo x =
    Json.Encode.object
        [ ( "eventId", Json.Encode.string x.eventId )
        , ( "eventName", Json.Encode.string x.eventName )
        ]