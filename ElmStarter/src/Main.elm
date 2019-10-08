module Main exposing (main)

import Browser exposing (Document)
import Flags exposing (Flags)
import Html as H


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { flags : Flags
    }


type Msg
    = NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
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


view : Model -> Document Msg
view model =
    { title = "TODO - Elm"
    , body = [ H.h1 [] [ H.text "Hello Elm!" ] ]
    }
