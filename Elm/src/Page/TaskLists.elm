module Page.TaskLists exposing (Model, Msg, init, subscriptions, update, view)

import Api.Lists
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Models.TaskList as TaskList exposing (TaskList)
import Models.TaskLists as TaskLists exposing (TaskLists)
import Models.Tasks exposing (Filter(..))
import RemoteData exposing (WebData)
import Routes
import Session exposing (Session)


type alias Model mainMsg =
    { session : Session
    , map : Msg -> mainMsg
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
    , Api.Lists.all session ListResult
        |> Cmd.map wrap
    )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions _ =
    Sub.none


type Msg
    = ListResult (Result Http.Error (List TaskList))
    | UpdateNeueListe String
    | SubmitNeueListe
    | NeueListeResult (Result Http.Error TaskList)
    | DeleteList TaskList.Id
    | DeleteListResult TaskList.Id (Result Http.Error ())


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    case msg of
        ListResult (Ok lists) ->
            ( { model | lists = RemoteData.Success (TaskLists.fromList lists) }, Cmd.none )

        ListResult (Err httpError) ->
            case httpError of
                Http.BadStatus 401 ->
                    ( { model | lists = RemoteData.Failure httpError }
                    , Session.navigateTo model Routes.Login
                        |> Cmd.map model.map
                    )

                _ ->
                    ( { model | lists = RemoteData.Failure httpError }, Cmd.none )

        UpdateNeueListe string ->
            ( { model | neueListe = string }, Cmd.none )

        SubmitNeueListe ->
            ( model
            , Api.Lists.add model.session NeueListeResult model.neueListe
                |> Cmd.map model.map
            )

        NeueListeResult (Ok liste) ->
            ( { model
                | neueListe = ""
                , lists =
                    model.lists
                        |> RemoteData.map (TaskLists.insertTaskList liste)
              }
            , Cmd.none
            )

        NeueListeResult (Err _) ->
            ( model, Cmd.none )

        DeleteList id ->
            ( model
            , Api.Lists.delete model.session (DeleteListResult id) id
                |> Cmd.map model.map
            )

        DeleteListResult id (Ok _) ->
            ( { model
                | lists =
                    model.lists
                        |> RemoteData.map (TaskLists.deleteTaskList id)
              }
            , Cmd.none
            )

        DeleteListResult _ (Err _) ->
            ( model, Cmd.none )


view : Model mainMsg -> Html mainMsg
view model =
    H.section
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
            , H.a [ Attr.href (Routes.routeToUrlString session.flags.baseUrlPath (Routes.List listItem.id All)) ]
                [ H.label [] [ H.text listItem.name ]
                ]
            , H.button
                [ Attr.class "destroy"
                , Ev.onClick (DeleteList listItem.id)
                ]
                []
            ]
        ]


listToDict : List TaskList -> Dict Int TaskList
listToDict list =
    list
        |> List.map (\i -> ( TaskList.idToInt i.id, i ))
        |> Dict.fromList
