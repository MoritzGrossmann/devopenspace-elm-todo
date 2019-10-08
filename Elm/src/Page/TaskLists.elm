module Page.TaskLists exposing (Model, Msg, init, subscriptions, update, view)

import Html as H exposing (Html)
import Session exposing (Session)


type alias Model mainMsg =
    { session : Session
    , map : Msg -> mainMsg
    }


init : (Msg -> mainMsg) -> Session -> ( Model mainMsg, Cmd mainMsg )
init wrap session =
    ( { session = session
      , map = wrap
      }
    , Cmd.none
    )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions _ =
    Sub.none


type Msg
    = NoOp


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    ( model, Cmd.none )


view : Model mainMsg -> Html mainMsg
view model =
    H.h1 [] [ H.text "Liste aller Tasks" ]
