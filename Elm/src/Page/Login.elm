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

import Api.Session
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Page
import RemoteData exposing (RemoteData, WebData)
import Session exposing (Login(..), Session)


type alias Model =
    { username : String
    , password : String
    , session : Session
    , token : WebData String
    }



----------------------------------------------------------------------------
-- Messages


type Msg
    = NoOp
    | UpdateUsername String
    | UpdatePassword String
    | Submit
    | LoginResult (Result Http.Error String)


type alias Page msg =
    Page.Page msg Model Msg


init : (Page.PageMsg Msg -> msg) -> Session -> ( Page msg, Cmd msg )
init wrapMsg session =
    let
        pageInit _ =
            ( { username = "", password = "", session = session, token = RemoteData.NotAsked }, Cmd.none )
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
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( { model | token = RemoteData.Loading }, Api.Session.post model LoginResult model.username model.password )

        LoginResult result ->
            case result of
                Ok token ->
                    ( { model | token = RemoteData.Success token, session = Session.updateLogin model.session (LoggedIn token) }, Cmd.none )

                Err error ->
                    ( { model | token = RemoteData.Failure error }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    H.section
        [ Attr.class "todoapp" ]
        [ H.h1 [] [ H.text "Login" ]
        , H.form [ Ev.onSubmit Submit ]
            [ H.input
                [ Attr.class "new-todo"
                , Attr.disabled (model.token |> RemoteData.isLoading)
                , Attr.placeholder "username"
                , Attr.autofocus True
                , Attr.value model.username
                , Ev.onInput UpdateUsername
                ]
                []
            , H.input
                [ Attr.class "new-todo"
                , Attr.disabled (model.token |> RemoteData.isLoading)
                , Attr.placeholder "password"
                , Attr.value model.password
                , Attr.type_ "password"
                , Ev.onInput UpdatePassword
                ]
                []
            , H.button [ Attr.style "display" "none", Ev.onClick Submit, Attr.type_ "submit" ] []
            ]
        ]
