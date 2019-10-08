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

import Auth
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Encode as Enc
import RemoteData exposing (WebData)
import Session exposing (Session)


type alias Model =
    { session : Session
    , username : String
    , password : String
    , result : WebData ()
    }


isLoading : Model -> Bool
isLoading model =
    RemoteData.isLoading model.result


isValidInput : Model -> Bool
isValidInput model =
    not (String.isEmpty <| String.trim model.username) && not (String.isEmpty <| String.trim model.password)



----------------------------------------------------------------------------
-- Messages


type Msg
    = NoOp
    | UpdateUsername String
    | UpdatePassword String
    | Submit
    | RemoteResult (Result Http.Error (Session -> Session))


init : Session -> ( Model, Cmd msg )
init session =
    let
        resetSession =
            Auth.clearAuthentication session
    in
    ( { session = resetSession
      , username = ""
      , password = ""
      , result = RemoteData.NotAsked
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        RemoteResult res ->
            case res of
                Ok updateSession ->
                    let
                        newSession =
                            updateSession model.session
                    in
                    ( { model | session = newSession, result = RemoteData.succeed () }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | result = RemoteData.Failure error }, Cmd.none )

        Submit ->
            if isValidInput model then
                ( { model | result = RemoteData.Loading }, Cmd.none )

            else
                ( model, Cmd.none )


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
                , Attr.disabled (isLoading model)
                , Attr.placeholder "username"
                , Attr.autofocus True
                , Attr.value model.username
                , Ev.onInput UpdateUsername
                ]
                []
            , H.input
                [ Attr.class "new-todo"
                , Attr.disabled (isLoading model)
                , Attr.placeholder "password"
                , Attr.value model.password
                , Attr.type_ "password"
                , Ev.onInput UpdatePassword
                ]
                []
            , viewSubmitButton model
            ]
        ]


viewSubmitButton : Model -> Html Msg
viewSubmitButton model =
    H.button [ Attr.style "display" "none", Attr.type_ "submit", Attr.disabled (not <| isValidInput model) ] []
