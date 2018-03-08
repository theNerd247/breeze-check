module UI exposing (..)

import ErrorMsg as Err
import EventName as EventName
import FindPeople as Find
import Html as Html exposing (Html)
import Nested exposing (modifyCmd)
import NewPerson as NewPerson
import Pages as Pages exposing (HasPageWrapper, pageWrapper)
import Pages.PhotoPage as PhotoPage exposing (HasPhotoPage)
import Pages.SearchPage as SearchPage exposing (HasSearchPage)
import Pages.SelectPage as SelectPage exposing (HasSelectPage)
import Pages.WaitingApprovalPage as WaitingApprovalPage exposing (HasWaitingApprovalPage)
import Router as Router


type alias Model =
    Router.HasRoutes (Find.HasFind (NewPerson.HasNewFamilies (EventName.HasEventName {})))


main =
    Router.mainWithRouter
        { init = ( model, EventName.GetEventName )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
        RouterMsg


type Msg
    = RouterMsg Router.Msg
    | EventNameMsg EventName.Msg
    | FindMsg Find.Msg
    | NewPersonMsg NewPerson.Msg
    | ErrMsg Err.Msg


model : Model
model =
    { errors = []
    , loadingStatus = False
    , groupId = Nothing
    , foundPeople = []
    , waitingCheckIn = []
    , searchLastName = ""
    , eventName = ""
    , personNotFound = False
    , newFamilies = NewPerson.initModel
    , currentRoute = Router.Search
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        FindMsg msg ->
            modifyCmd FindMsg <| Find.update msg mdl

        ErrMsg msg ->
            ( Err.update msg mdl, Cmd.none )

        NewPersonMsg msg ->
            let
                f m ps =
                    { m
                        | waitingCheckIn =
                            ps
                                |> List.map (\p -> { p | checkedIn = True })
                                |> List.append m.waitingCheckIn
                        , searchLastName = ""
                        , personNotFound = False
                    }
            in
            modifyCmd NewPersonMsg <| NewPerson.update f msg mdl

        _ ->
            ( mdl, Cmd.none )


view : Model -> Html Msg
view mdl =
    Pages.pageWrapper mdl ErrMsg <|
        case mdl.currentRoute of
            Router.Search ->
                Html.map SearchPageMsg <| SearchPage.view mdl

            Router.Selected ->
                Html.map SelectPageMsg <| SelectPage.view mdl

            Router.Photo ->
                Html.map PhotoPageMsg <| PhotoPage.view mdl

            Router.WaitingApproval ->
                Html.map WaitingApprovalPageMsg <| WaitingApprovalPage.view mdl
