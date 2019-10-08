module Login exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

----------------------------------------------------------------------------
-- Model

import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Session exposing (Session)


type alias Model =
    { session : Session
    }



----------------------------------------------------------------------------
-- Messages


type Msg
    = NoOp


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    H.h1 [] [ H.text "LOGIN" ]
