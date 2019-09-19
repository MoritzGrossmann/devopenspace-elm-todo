module Page.Login exposing
    ( Model
    , Msg(..)
    , Page
    , init
    , subscriptions
    , update
    , view
    )

----------------------------------------------------------------------------
-- Model

import Html as H exposing (Html)
import Http
import Page
import Session exposing (Session)


type alias Model =
    { username : String
    , password : String
    , session : Session
    }



----------------------------------------------------------------------------
-- Messages


type Msg
    = NoOp
    | UpdateUsername String
    | UpdatePassword String
    | Submit
    | LoginResult (Result Http.Error ())


type alias Page msg =
    Page.Page msg Model Msg


init : (Page.PageMsg Msg -> msg) -> Session -> ( Page msg, Cmd msg )
init wrapMsg session =
    let
        pageInit _ =
            ( { username = "", password = "", session = session }, Cmd.none )
    in
    Page.init wrapMsg pageInit view update subscriptions session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | username = password }, Cmd.none )

        Submit ->
            ( model, Cmd.none )

        LoginResult result ->
            case result of
                Ok value ->
                    ( model, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    H.div [] []
