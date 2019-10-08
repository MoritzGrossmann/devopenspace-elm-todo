module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Flags exposing (Flags)
import Login
import Session exposing (Session)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( session, sessionCmd ) =
            Session.init flags
    in
    let
        ( model, cmd ) =
            Login.init session
    in
    ( model
    , Cmd.batch
        [ cmd
        , sessionCmd
        ]
    )


type alias Model =
    Login.Model


type alias Msg =
    Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Login.update msg model


view : Model -> Document Msg
view model =
    let
        page =
            Login.view model
    in
    { title = "TODO - Elm"
    , body = [ page ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Login.subscriptions model


updateSession : (Session -> Session) -> Model -> Model
updateSession upd model =
    { model | session = upd model.session }


withSession : (Session -> a) -> Model -> a
withSession with model =
    with model.session


getSession : Model -> Session
getSession =
    withSession identity
