module Page.LoginPending exposing (Model, Msg, init, subscriptions, update, view)

import Auth
import Debug
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
    , Auth.requestLocalStorageAuth
    )


type Msg
    = NoOp
    | LocalStorageAuthReceived (Session -> Session)


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LocalStorageAuthReceived updateSession ->
            let
                newSession =
                    updateSession model.session

                navCmd =
                    if Auth.isAuthenticated newSession then
                        Cmd.map model.map (Session.navigateTo model (model.route |> Maybe.withDefault Routes.Lists))

                    else
                        Cmd.map model.map (Session.navigateTo model (model.route |> Maybe.withDefault Routes.Login))
            in
            ( { model | session = newSession }, navCmd )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions model =
    Sub.map model.map (Auth.watchLocalStorage NoOp LocalStorageAuthReceived)


view : Model msg -> Html msg
view _ =
    H.div [] [ H.text "login pending..." ]
