module UI exposing (..)

import FindPeople as Find
import Html as Html exposing (Html)
import Nested exposing (modifyCmd)
import NewPerson as NewPerson
import PageWrapper as PageWrapper exposing (HasPageWrapper)
import Pages.PhotoPage as PhotoPage exposing (HasPhotoPage)
import Pages.SearchPage as SearchPage exposing (HasSearchPage)
import Pages.SelectPage as SelectPage exposing (HasSelectPage)
import Pages.WaitingApprovalPage as WaitingApprovalPage exposing (HasWaitingApprovalPage)
import Router as Router exposing (HasRoutes, mainWithRouter)


type alias Model =
    HasRoutes (HasPageWrapper (Find.HasFind (NewPerson.HasNewFamilies {})))


type Msg
    = RouterMsg Router.Msg
    | PageWrapper PageWrapper.Msg
    | SearchPage SearchPage.Msg
    | SelectPage SelectPage.Msg
    | PhotoPage PhotoPage.Msg
    | WaitingApprovalPage WaitingApprovalPage.Msg


main =
    --Html.program
    mainWithRouter
        { init = modifyCmd PageWrapper <| PageWrapper.initPages model
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
        RouterMsg


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
        RouterMsg msg ->
            modifyCmd RouterMsg <| Router.update msg mdl

        PageWrapper msg ->
            modifyCmd PageWrapper <| PageWrapper.update msg mdl

        SearchPage msg ->
            modifyCmd SearchPage <| SearchPage.update msg mdl

        SelectPage msg ->
            modifyCmd SelectPage <| SelectPage.update msg mdl

        PhotoPage msg ->
            modifyCmd PhotoPage <| PhotoPage.update msg mdl

        WaitingApprovalPage msg ->
            modifyCmd WaitingApprovalPage <| WaitingApprovalPage.update msg mdl


view : Model -> Html Msg
view mdl =
    PageWrapper.view PageWrapper mdl <|
        case mdl.currentRoute of
            Router.Search ->
                Html.map SearchPage <| SearchPage.view mdl

            Router.Selected ->
                Html.map SelectPage <| SelectPage.view mdl

            Router.Photo ->
                Html.map PhotoPage <| PhotoPage.view mdl

            Router.WaitingApproval ->
                Html.map WaitingApprovalPage <| WaitingApprovalPage.view mdl
