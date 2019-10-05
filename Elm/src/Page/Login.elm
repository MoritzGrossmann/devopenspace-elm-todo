module Page.Login exposing
    ( Model
    , Msg
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
import Json.Encode as Enc
import LocalStorage
import Page
import RemoteData exposing (WebData)
import Routes exposing (Route)
import Session exposing (Login(..), Session)


type alias Model mainMsg =
    { username : String
    , password : String
    , session : Session
    , token : WebData String
    , transitionTo : Maybe Route
    , map : Msg -> mainMsg
    }



----------------------------------------------------------------------------
-- Messages


type Msg
    = NoOp
    | UpdateUsername String
    | UpdatePassword String
    | Submit
    | LoginResult (Result Http.Error String)


init : (Msg -> mainMsg) -> Session -> Maybe Route -> ( Model mainMsg, Cmd msg )
init wrapMsg session transitionTo =
    ( { username = ""
      , password = ""
      , session = session
      , token = RemoteData.NotAsked
      , transitionTo = transitionTo
      , map = wrapMsg
      }
    , Cmd.none
    )


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( { model | token = RemoteData.Loading }
            , Cmd.map model.map (Api.Session.post model.session LoginResult model.username model.password)
            )

        LoginResult result ->
            case result of
                Ok token ->
                    ( { model
                        | token = RemoteData.Success token
                        , session = Session.updateLogin model.session (LoggedIn token)
                      }
                    , Cmd.batch
                        [ Session.navigateTo model (model.transitionTo |> Maybe.withDefault Routes.Lists)
                        , LocalStorage.store ( LocalStorage.authorizationKey, Just (token |> Enc.string) )
                        ]
                        |> Cmd.map model.map
                    )

                Err error ->
                    ( { model | token = RemoteData.Failure error }, Cmd.none )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions _ =
    Sub.none


view : Model mainMsg -> Html mainMsg
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
            , H.button [ Attr.style "display" "none", Attr.type_ "submit" ] []
            ]
        ]
        |> H.map model.map
