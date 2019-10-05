module Page.LoginPending exposing (Model, Msg, init, subscriptions, update, view)

import Html as H exposing (Html)
import LocalStorage
import Routes exposing (Route)
import Session exposing (Session)


type alias Model mainMsg =
    { route : Maybe Route
    , session : Session
    , map : Msg -> mainMsg
    }


init : (Msg -> mainMsg) -> Session -> Maybe Route -> ( Model mainMsg, Cmd mainMsg )
init mapMsg session route =
    ( { session = session
      , route = route
      , map = mapMsg
      }
    , Cmd.map mapMsg (LocalStorage.request LocalStorage.authorizationKey)
    )


type Msg
    = GotLocalStorageItem ( LocalStorage.StorageKey, Maybe String )


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    case msg of
        GotLocalStorageItem ( key, Just token ) ->
            if key == LocalStorage.authorizationKey then
                ( { model | session = Session.updateLogin model.session (Session.LoggedIn token) }
                , Cmd.map model.map (Session.navigateTo model (model.route |> Maybe.withDefault Routes.Lists))
                )

            else
                ( model, Cmd.none )

        GotLocalStorageItem ( key, Nothing ) ->
            if key == LocalStorage.authorizationKey then
                ( model, Cmd.map model.map (Session.navigateTo model Routes.Login) )

            else
                ( model, Cmd.none )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions model =
    Sub.map model.map (LocalStorage.receive GotLocalStorageItem)


view : Model msg -> Html msg
view _ =
    H.div [] [ H.text "login pending..." ]
