module Page.LoginPending exposing (Model, Msg, Page, init, subscriptions, update)

import Html as H exposing (Html)
import LocalStorage
import Page
import Routes exposing (Route)
import Session exposing (Session)


type alias Page msg =
    Page.Page msg Model Msg


type alias Model =
    { route : Maybe Route
    , session : Session
    }


init : (Page.PageMsg Msg -> msg) -> Session -> Maybe Route -> ( Page msg, Cmd msg )
init wrap session route =
    let
        pageInit _ =
            ( { session = session
              , route = route
              }
            , LocalStorage.request LocalStorage.authorizationKey
            )
    in
    Page.init wrap pageInit view update subscriptions session


type Msg
    = GotLocalStorageItem ( LocalStorage.StorageKey, Maybe String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLocalStorageItem ( key, Just token ) ->
            if key == LocalStorage.authorizationKey then
                ( { model | session = Session.updateLogin model.session (Session.LoggedIn token) }
                , Session.navigateTo model (model.route |> Maybe.withDefault Routes.Login)
                )

            else
                ( model, Cmd.none )

        GotLocalStorageItem ( key, Nothing ) ->
            if key == LocalStorage.authorizationKey then
                ( model, Session.navigateTo model Routes.Login )

            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    LocalStorage.receive GotLocalStorageItem


view : Model -> Html Msg
view _ =
    H.div [] [ H.text "login pending..." ]
