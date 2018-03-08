module UI exposing (..)

import NewPerson as NewPerson
import Pages as Page
import Pages.NewPersonsPage exposing (..)
import Pages.PhotoPage exposing (..)
import Pages.SearchPage exposing (..)
import Pages.SelectPage exposing (..)
import Pages.WaitingApprovalPage exposing (..)


type Pages
    = Search
    | Select
    | NewPersons
    | Finished
    | Photo


type alias Model =
    Page.HasMainPage
        (NewPerson.HasNewFamilies
            { page : Pages
            }
        )


type Msg
    = Find Find.Msg
    | Err Err.Msg
    | NewPerson NewPerson.Msg
    | SetPage Page
    | GetEventName
    | EventNameResult (BreezeApi.Msg Data.EventName)


main : Program Never Model Msg
main =
    program
        { init = getEventName model
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


app : Url.App Model Msg
app =
    { init = getEventName model
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    , delta2url = delta2url
    , location2messages = location2messages
    }


model : Model
model =
    { errors = []
    , loadingStatus = False
    , groupId = Nothing
    , foundPeople = []
    , waitingCheckIn = []
    , searchLastName = ""
    , page = Search
    , eventName = ""
    , personNotFound = False
    , newFamilies = NewPerson.initModel
    }



-- UPDATE


delta2url : Model -> Model -> Maybe Url.UrlChange
delta2url old current =
    Nothing


location2messages : Nav.Location -> List Msg
location2messages l =
    []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        mdl =
            m
                |> pageUpdate msg
                |> setPageUpdate msg
    in
    case msg of
        Find msg ->
            modifyCmd Find <| Find.update msg mdl

        Err msg ->
            ( Err.update msg mdl, Cmd.none )

        NewPerson msg ->
            let
                f m ps =
                    { m
                        | waitingCheckIn =
                            ps
                                |> List.map (\p -> { p | checkedIn = True })
                                |> List.append m.waitingCheckIn
                        , page = Select
                        , searchLastName = ""
                        , personNotFound = False
                    }
            in
            modifyCmd NewPerson <| NewPerson.update f msg mdl

        _ ->
            ( mdl, Cmd.none )


getEventName : Model -> ( Model, Cmd Msg )
getEventName mdl =
    BreezeApi.eventInfo EventNameResult mdl


pageUpdate : Msg -> Model -> Model
pageUpdate msg mdl =
    case mdl.page of
        Search ->
            searchPageUpdate msg mdl

        Select ->
            selectPageUpdate msg mdl

        Finished ->
            finishedPageUpdate msg mdl

        Photo ->
            photoPageUpdate msg mdl

        _ ->
            mdl


setPageUpdate : Msg -> Model -> Model
setPageUpdate msg mdl =
    case msg of
        SetPage p ->
            { mdl | page = p }

        _ ->
            mdl



-- UPDATE functions to occur only when on specific pages


searchPageUpdate : Msg -> Model -> Model
searchPageUpdate msg mdl =
    case msg of
        Find Find.SearchClick ->
            { mdl | page = Select }

        _ ->
            mdl


selectPageUpdate : Msg -> Model -> Model
selectPageUpdate msg mdl =
    case msg of
        Find (Find.CheckInResponse (BreezeApi.Recieved (Ok (Ok _)))) ->
            { mdl | page = Photo }

        _ ->
            mdl


finishedPageUpdate : Msg -> Model -> Model
finishedPageUpdate msg mdl =
    case msg of
        Find Find.CancelCheckInClick ->
            { mdl | page = Select }

        _ ->
            mdl


photoPageUpdate : Msg -> Model -> Model
photoPageUpdate msg mdl =
    case msg of
        Find Find.CheckInClick ->
            { mdl | page = Finished }

        _ ->
            mdl



-- VIEW


view : Model -> Html Msg
view mdl =
    let
        titleRow =
            [ Grid.row [ Row.centerLg ]
                [ Grid.col [ Col.lg8, Col.attrs [ class "text-center" ] ]
                    [ h1 [] [ text mdl.eventName ]
                    , p [] [ text "Mountain View Church" ]
                    ]
                ]
            ]

        errors =
            [ Html.map Err <| Err.view mdl
            ]

        page =
            case mdl.page of
                Search ->
                    searchPageView mdl

                Select ->
                    selectPageView mdl

                Finished ->
                    finishedPageView mdl

                NewPersons ->
                    newPersonsView mdl

                Photo ->
                    photoView mdl

        body =
            page
                |> List.append errors
                |> List.append titleRow
    in
    Grid.containerFluid [ class "clearfix" ]
        [ Grid.containerFluid [ class "mb-5" ] body

        --, tabs
        ]
