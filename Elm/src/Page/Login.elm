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

import Auth
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Encode as Enc
import LocalStorage
import Navigation.Routes as Routes exposing (Route)
import RemoteData exposing (WebData)
import Session exposing (Session)


type alias Model mainMsg =
    { username : String
    , password : String
    , session : Session
    , login : WebData ()
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
    | LoginResult (Result Http.Error (Session -> Session))


init : (Msg -> mainMsg) -> Session -> Maybe Route -> ( Model mainMsg, Cmd msg )
init wrapMsg session transitionTo =
    let
        resetedSession =
            Auth.clearAuthentication session
    in
    ( { username = ""
      , password = ""
      , session = resetedSession
      , login = RemoteData.NotAsked
      , transitionTo = transitionTo
      , map = wrapMsg
      }
    , Auth.updateLocalStorage resetedSession.authentication
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
            ( { model | login = RemoteData.Loading }
            , Cmd.map model.map (Auth.httpLogin model.session.flags LoginResult model.username model.password)
            )

        LoginResult res ->
            case res of
                Ok updateSession ->
                    let
                        newSession =
                            updateSession model.session
                    in
                    ( { model
                        | session = newSession
                        , login = RemoteData.succeed ()
                      }
                    , Cmd.batch
                        [ Routes.navigateTo model.session (model.transitionTo |> Maybe.withDefault Routes.Lists)
                        , Auth.updateLocalStorage newSession.authentication
                        ]
                        |> Cmd.map model.map
                    )

                Err error ->
                    ( { model | login = RemoteData.Failure error }, Cmd.none )


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
                , Attr.disabled (RemoteData.isLoading model.login)
                , Attr.placeholder "username"
                , Attr.autofocus True
                , Attr.value model.username
                , Ev.onInput UpdateUsername
                ]
                []
            , H.input
                [ Attr.class "new-todo"
                , Attr.disabled (RemoteData.isLoading model.login)
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
