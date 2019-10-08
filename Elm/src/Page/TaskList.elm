module Page.TaskList exposing (Model, Msg, init, subscriptions, update, view)

import Api.TaskList as ApiList
import Browser.Dom as Dom
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Decode as Json
import Models.TaskList as TaskList exposing (TaskList)
import Navigation.Routes as Routes
import RemoteData exposing (WebData)
import Session exposing (Session)
import Task


type alias Model mainMsg =
    { session : Session
    , map : Msg -> mainMsg
    }


type Msg
    = NoOp


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


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model mainMsg -> Html mainMsg
view model =
    viewTaskApp model
        |> H.map model.map


viewTaskApp : Model mainMsg -> Html Msg
viewTaskApp model =
    H.div []
        [ H.section
            [ Attr.class "todoapp" ]
            [ viewHeader model
            , viewMain model
            , viewFooter model
            ]
        ]


viewHeader : Model mainMsg -> Html Msg
viewHeader model =
    H.header
        [ Attr.class "header" ]
        [ H.h1 []
            [ H.text "Tasks.."
            ]
        , H.input
            [ Attr.class "new-todo"
            , Attr.placeholder "what needs to be done?"
            , Attr.autofocus True
            ]
            []
        ]


{-| should be hidden by default and shown when there are tasks
-}
viewMain : Model mainMsg -> Html Msg
viewMain model =
    H.section
        [ Attr.class "main" ]
        [ H.input
            [ Attr.id "toggle-all"
            , Attr.class "toggle-all"
            , Attr.type_ "checkbox"
            ]
            []
        , H.label
            [ Attr.for "toggle-all" ]
            [ H.text "Mark all as complete" ]

        -- li-tasks with class completed (if it is), containing viewTask or editTask
        , H.ul
            [ Attr.class "todo-list" ]
            []
        ]


{-| should be hidden by default and shown when there are tasks
-}
viewFooter : Model mainMsg -> Html Msg
viewFooter model =
    H.footer
        [ Attr.class "footer" ]
        [ H.span [ Attr.class "todo-count" ]
            [ H.strong [] [ H.text "0" ]
            , H.text " task left"
            ]

        -- Achtung: Links sind so nicht richtig ;)
        , H.ul [ Attr.class "filters" ]
            [ H.li []
                [ H.a
                    [ Attr.classList [ ( "selected", True ) ]
                    , Attr.href (Routes.routeToUrlString model.session.flags.baseUrlPath Routes.Lists)
                    ]
                    [ H.text "All" ]
                ]
            , H.li []
                [ H.a
                    [ Attr.classList [ ( "selected", False ) ]
                    , Attr.href (Routes.routeToUrlString model.session.flags.baseUrlPath Routes.Lists)
                    ]
                    [ H.text "Active" ]
                ]
            , H.li []
                [ H.a
                    [ Attr.classList [ ( "selected", False ) ]
                    , Attr.href (Routes.routeToUrlString model.session.flags.baseUrlPath Routes.Lists)
                    ]
                    [ H.text "Completed" ]
                ]
            ]
        , H.button
            [ Attr.class "clear-completed"
            ]
            [ H.text "Clear completed" ]
        ]


onFinish : Msg -> Msg -> H.Attribute Msg
onFinish enterMessage escapeMessage =
    let
        select key =
            case key of
                13 ->
                    enterMessage

                27 ->
                    escapeMessage

                _ ->
                    -- Not a 'finish' key, such as ENTER or ESCAPE
                    NoOp
    in
    Ev.on "keydown" (Json.map select Ev.keyCode)
