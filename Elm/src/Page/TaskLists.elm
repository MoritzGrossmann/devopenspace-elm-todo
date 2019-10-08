module Page.TaskLists exposing (Model, Msg, init, subscriptions, update, view)

import Api.TaskList as Api
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Models.TaskList as TaskList exposing (TaskList)
import Models.TaskLists as TaskLists exposing (TaskLists)
import Navigation.Routes as Routes
import RemoteData exposing (WebData)
import Session exposing (Session)


type alias Model mainMsg =
    { session : Session
    , map : Msg -> mainMsg

    -- nimmt die Liste mit Tasks auf
    , lists : WebData TaskLists
    , neueListe : String
    }


init : (Msg -> mainMsg) -> Session -> ( Model mainMsg, Cmd mainMsg )
init wrap session =
    ( { session = session
      , map = wrap
      , lists = RemoteData.Loading
      , neueListe = ""
      }
      -- TODO: Liste laden lassen
    , Cmd.none
    )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions _ =
    Sub.none


type Msg
    = UpdateNeueListe String
    | SubmitNeueListe
    | DeleteList TaskList.Id


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    case msg of
        UpdateNeueListe string ->
            ( { model | neueListe = string }, Cmd.none )

        SubmitNeueListe ->
            ( model
              -- TODO Neue Liste an Backend übertragen
            , Cmd.none
            )

        DeleteList id ->
            ( model
              -- TODO Liste im Backend löschen
            , Cmd.none
            )


view : Model mainMsg -> Html mainMsg
view model =
    H.div []
        [ H.section
            [ Attr.class "todoapp" ]
            [ H.h1 [] [ H.text "Listen" ]
            , H.form [ Ev.onSubmit SubmitNeueListe ]
                [ H.input
                    [ Attr.class "new-todo"
                    , Attr.placeholder "create a List"
                    , Attr.autofocus True
                    , Attr.value model.neueListe
                    , Ev.onInput UpdateNeueListe
                    ]
                    []
                , H.button [ Attr.style "display" "none", Attr.type_ "submit" ] []
                ]
            , H.ul
                [ Attr.class "todo-list" ]
                (model.lists
                    |> RemoteData.map
                        (TaskLists.allTaskLists >> List.map (viewListItem model.session))
                    |> RemoteData.withDefault []
                )
            ]
        ]
        |> H.map model.map


viewListItem : Session -> TaskList -> Html Msg
viewListItem session listItem =
    H.li [ Attr.class "todo" ]
        [ H.div
            [ Attr.class "view"
            ]
            [ H.div
                [ Attr.class "open-count" ]
                [ H.text (listItem.active |> String.fromInt) ]

            -- TODO Href setzen
            , H.a []
                [ H.label [] [ H.text listItem.name ]
                ]
            , H.button
                [ Attr.class "destroy"
                , Ev.onClick (DeleteList listItem.id)
                ]
                []
            ]
        ]
