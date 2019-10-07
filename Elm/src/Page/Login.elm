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
    { session : Session
    , map : Msg -> mainMsg
    , transitionTo : Maybe Route
    , modus : Modus
    , result : WebData ()
    }


type Modus
    = Login LoginModel
    | Register RegisterModel


type alias LoginModel =
    { username : String
    , password : String
    }


type alias RegisterModel =
    { username : String
    , password : String
    , repeatPassword : String
    }


isLoading : Model mainMsg -> Bool
isLoading model =
    RemoteData.isLoading model.result


isValidInput : Model mainMsg -> Bool
isValidInput model =
    case model.modus of
        Login loginModel ->
            not (String.isEmpty <| String.trim loginModel.username) && not (String.isEmpty <| String.trim loginModel.password)

        Register registerModel ->
            not (String.isEmpty <| String.trim registerModel.username) && not (String.isEmpty <| String.trim registerModel.password) && registerModel.password == registerModel.repeatPassword


emptyLogin : LoginModel
emptyLogin =
    { username = ""
    , password = ""
    }


emptyRegister : RegisterModel
emptyRegister =
    { username = ""
    , password = ""
    , repeatPassword = ""
    }



----------------------------------------------------------------------------
-- Messages


type Msg
    = NoOp
    | UpdateUsername String
    | UpdatePassword String
    | UpdateRepeatPassword String
    | Submit
    | RemoteResult (Result Http.Error (Session -> Session))
    | SwitchToRegister
    | SwitchToLogin


init : (Msg -> mainMsg) -> Session -> Maybe Route -> ( Model mainMsg, Cmd msg )
init wrapMsg session transitionTo =
    let
        resetedSession =
            Auth.clearAuthentication session
    in
    ( { session = resetedSession
      , map = wrapMsg
      , transitionTo = transitionTo
      , modus = Login emptyLogin
      , result = RemoteData.NotAsked
      }
    , Auth.updateLocalStorage resetedSession.authentication
    )


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    let
        ( changedMain, mainCmd ) =
            updateMain msg model
    in
    case changedMain.modus of
        Login loginModel ->
            let
                ( newLoginModel, loginCmd ) =
                    updateLogin changedMain msg loginModel
            in
            ( { changedMain | modus = Login newLoginModel }, Cmd.batch [ mainCmd, loginCmd ] )

        Register registerModel ->
            let
                ( newRegisterModel, registerCmd ) =
                    updateRegister changedMain msg registerModel
            in
            ( { changedMain | modus = Register newRegisterModel }, Cmd.batch [ mainCmd, registerCmd ] )


updateMain : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
updateMain msg model =
    case msg of
        SwitchToLogin ->
            ( { model
                | result = RemoteData.NotAsked
                , modus = Login emptyLogin
              }
            , Cmd.none
            )

        SwitchToRegister ->
            ( { model
                | result = RemoteData.NotAsked
                , modus = Register emptyRegister
              }
            , Cmd.none
            )

        RemoteResult res ->
            case res of
                Ok updateSession ->
                    let
                        newSession =
                            updateSession model.session
                    in
                    ( { model | session = newSession, result = RemoteData.succeed () }
                    , Cmd.batch
                        [ Routes.navigateTo model.session (model.transitionTo |> Maybe.withDefault Routes.Lists)
                        , Auth.updateLocalStorage newSession.authentication
                        ]
                        |> Cmd.map model.map
                    )

                Err error ->
                    ( { model | result = RemoteData.Failure error }, Cmd.none )

        Submit ->
            if isValidInput model then
                ( { model | result = RemoteData.Loading }, Cmd.none )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateLogin : Model mainMsg -> Msg -> LoginModel -> ( LoginModel, Cmd mainMsg )
updateLogin mainModel msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        UpdateRepeatPassword _ ->
            ( model, Cmd.none )

        Submit ->
            if isValidInput mainModel then
                ( model, Cmd.map mainModel.map (Auth.httpLogin mainModel.session.flags RemoteResult model.username model.password) )

            else
                ( model, Cmd.none )

        RemoteResult _ ->
            ( model, Cmd.none )

        SwitchToRegister ->
            ( model, Cmd.none )

        SwitchToLogin ->
            ( model, Cmd.none )


updateRegister : Model mainMsg -> Msg -> RegisterModel -> ( RegisterModel, Cmd mainMsg )
updateRegister mainModel msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        UpdateRepeatPassword password ->
            ( { model | repeatPassword = password }, Cmd.none )

        Submit ->
            if isValidInput mainModel then
                ( model, Cmd.map mainModel.map (Auth.httpRegister mainModel.session.flags RemoteResult model.username model.password) )

            else
                ( model, Cmd.none )

        RemoteResult _ ->
            ( model, Cmd.none )

        SwitchToRegister ->
            ( model, Cmd.none )

        SwitchToLogin ->
            ( model, Cmd.none )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions _ =
    Sub.none


view : Model mainMsg -> Html mainMsg
view model =
    H.section
        [ Attr.class "todoapp" ]
        (case model.modus of
            Login loginModel ->
                viewLogin model loginModel

            Register registerModel ->
                viewRegister model registerModel
        )
        |> H.map model.map


viewLogin : Model mainMsg -> LoginModel -> List (Html Msg)
viewLogin mainModel loginModel =
    [ H.section
        [ Attr.class "todoapp" ]
        [ H.h1 [] [ H.text "Login" ]
        , H.form [ Ev.onSubmit Submit ]
            [ H.input
                [ Attr.class "new-todo"
                , Attr.disabled (isLoading mainModel)
                , Attr.placeholder "username"
                , Attr.autofocus True
                , Attr.value loginModel.username
                , Ev.onInput UpdateUsername
                ]
                []
            , H.input
                [ Attr.class "new-todo"
                , Attr.disabled (isLoading mainModel)
                , Attr.placeholder "password"
                , Attr.value loginModel.password
                , Attr.type_ "password"
                , Ev.onInput UpdatePassword
                ]
                []
            , viewSubmitButton mainModel
            ]
        ]
    , H.h2 [] [ H.button [ Attr.class "clear-completed", Ev.onClick SwitchToRegister ] [ H.text "register" ] ]
    ]


viewRegister : Model mainMsg -> RegisterModel -> List (Html Msg)
viewRegister mainModel registerModel =
    [ H.section
        [ Attr.class "todoapp" ]
        [ H.h1 [] [ H.text "Register" ]
        , H.form [ Ev.onSubmit Submit ]
            [ H.input
                [ Attr.class "new-todo"
                , Attr.disabled (isLoading mainModel)
                , Attr.placeholder "username"
                , Attr.autofocus True
                , Attr.value registerModel.username
                , Ev.onInput UpdateUsername
                ]
                []
            , H.input
                [ Attr.class "new-todo"
                , Attr.disabled (isLoading mainModel)
                , Attr.placeholder "password"
                , Attr.value registerModel.password
                , Attr.type_ "password"
                , Ev.onInput UpdatePassword
                ]
                []
            , H.input
                [ Attr.class "new-todo"
                , Attr.disabled (isLoading mainModel)
                , Attr.placeholder "repeat password"
                , Attr.value registerModel.repeatPassword
                , Attr.type_ "password"
                , Ev.onInput UpdateRepeatPassword
                ]
                []
            , viewSubmitButton mainModel
            ]
        ]
    , H.h2 [] [ H.button [ Attr.class "clear-completed", Ev.onClick SwitchToLogin ] [ H.text "login" ] ]
    ]


viewSubmitButton : Model mainMsg -> Html Msg
viewSubmitButton mainModel =
    H.button [ Attr.style "display" "none", Attr.type_ "submit", Attr.disabled (not <| isValidInput mainModel) ] []
