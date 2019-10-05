module Page.List exposing (Model, Msg, init, subscriptions, update, view)

import Api.Lists
import Api.Tasks as Api
import Browser.Dom as Dom
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Decode as Json
import Models.Task as Task exposing (Task)
import Models.TaskList as TaskList exposing (TaskList)
import Models.Tasks as Tasks exposing (Filter(..), Tasks)
import Page
import RemoteData exposing (WebData)
import Routes
import Session exposing (Session)
import Task


type alias Model mainMsg =
    { session : Session
    , map : Msg -> mainMsg
    , todos : Tasks
    , newText : String
    , editingTask :
        Maybe
            { id : Task.Id
            , text : String
            }
    , activeFilter : Filter
    , listId : TaskList.Id
    , taskList : WebData TaskList
    }


type Msg
    = NoOp
    | UpdateNewText String
    | ClearNewText
    | AddTodo
    | CheckTask Task.Id Bool
    | DeleteTask Task.Id
    | EditTask Task
    | UpdateEditText String
    | FinishEdit
    | CancelEdit
    | ClearCompleted
    | ToggleAll Bool
    | TasksReceived (Result Http.Error (List Task))
    | TaskReceived (Result Http.Error Task)
    | ListMetaDataReceived (Result Http.Error TaskList)


init : (Msg -> mainMsg) -> Session -> Filter -> TaskList.Id -> ( Model mainMsg, Cmd mainMsg )
init wrap session filter listId =
    ( { session = session
      , map = wrap
      , todos = Tasks.empty
      , newText = ""
      , editingTask = Nothing
      , activeFilter = filter
      , taskList = RemoteData.Loading
      , listId = listId
      }
    , Cmd.batch
        [ Api.getAll session TasksReceived listId
        , Api.Lists.byId session ListMetaDataReceived listId
        ]
        |> Cmd.map wrap
    )


subscriptions : Model mainMsg -> Sub mainMsg
subscriptions _ =
    Sub.none


activeCount : Tasks -> Int
activeCount todos =
    List.length (todos |> Tasks.activeTasks)


filtered : Filter -> Tasks -> List Task
filtered filter =
    case filter of
        All ->
            Tasks.allTasks

        Active ->
            Tasks.activeTasks

        Completed ->
            Tasks.completedTasks


update : Msg -> Model mainMsg -> ( Model mainMsg, Cmd mainMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateNewText updatedText ->
            ( { model | newText = updatedText }, Cmd.none )

        ClearNewText ->
            ( { model | newText = "" }, Cmd.none )

        AddTodo ->
            let
                insertCmd =
                    Api.new model.session TaskReceived model.listId model.newText
                        |> Cmd.map model.map
            in
            ( { model | newText = "" }, insertCmd )

        CheckTask taskId completed ->
            let
                ( newTask, newTasks ) =
                    Tasks.setCompleted completed taskId model.todos

                updateCmd =
                    newTask
                        |> Maybe.map (Api.update model.session TaskReceived model.listId)
                        |> Maybe.withDefault Cmd.none
                        |> Cmd.map model.map
            in
            ( { model | todos = newTasks }, updateCmd )

        DeleteTask taskId ->
            let
                deleteCmd =
                    Api.delete model.session TasksReceived taskId
                        |> Cmd.map model.map

                newTasks =
                    Tasks.deleteTask taskId model.todos
            in
            ( { model | todos = newTasks }, deleteCmd )

        EditTask task ->
            let
                edit =
                    { id = task.id
                    , text = task.text
                    }
            in
            ( { model | editingTask = Just edit }
            , Task.attempt (always NoOp) (Dom.focus ("edit_" ++ Task.idToString task.id))
                |> Cmd.map model.map
            )

        UpdateEditText updatedText ->
            let
                edit =
                    model.editingTask
                        |> Maybe.map (\task -> { task | text = updatedText })
            in
            ( { model | editingTask = edit }, Cmd.none )

        CancelEdit ->
            ( { model | editingTask = Nothing }, Cmd.none )

        FinishEdit ->
            let
                ( updatedTask, newTasks ) =
                    model.editingTask
                        |> Maybe.map
                            (\editingTask ->
                                Tasks.setText editingTask.text editingTask.id model.todos
                            )
                        |> Maybe.withDefault ( Nothing, model.todos )

                updateCmd =
                    updatedTask
                        |> Maybe.map (Api.update model.session TaskReceived model.listId)
                        |> Maybe.withDefault Cmd.none
                        |> Cmd.map model.map
            in
            ( { model
                | todos = newTasks
                , editingTask = Nothing
              }
            , updateCmd
            )

        ClearCompleted ->
            let
                delete todos id =
                    Tasks.deleteTask id todos

                ( deleteCmds, deletedTasks ) =
                    Tasks.completedTasks model.todos
                        |> List.foldl
                            (\task ( cmds, _ ) ->
                                let
                                    newTasks =
                                        Tasks.deleteTask task.id model.todos

                                    deleteCmd =
                                        Api.delete model.session TasksReceived task.id
                                in
                                ( deleteCmd :: cmds, newTasks )
                            )
                            ( [], model.todos )
            in
            ( { model | todos = deletedTasks }, Cmd.batch deleteCmds |> Cmd.map model.map )

        ToggleAll setCompleted ->
            let
                toggle todos id =
                    Tasks.setCompleted setCompleted id todos

                ( toggleCmds, toggledTasks ) =
                    filtered model.activeFilter model.todos
                        |> List.foldl
                            (\task ( cmds, todos ) ->
                                let
                                    ( toggledTask, newTasks ) =
                                        toggle todos task.id

                                    toggleCmd =
                                        toggledTask
                                            |> Maybe.map (Api.update model.session TaskReceived model.listId)
                                            |> Maybe.withDefault Cmd.none
                                in
                                ( toggleCmd :: cmds, newTasks )
                            )
                            ( [], model.todos )
            in
            ( { model | todos = toggledTasks }, Cmd.batch toggleCmds |> Cmd.map model.map )

        TasksReceived (Ok tasks) ->
            ( { model | todos = Tasks.fromList tasks }, Cmd.none )

        TasksReceived (Err _) ->
            -- for this Demo we ignore communication errors - sorry
            ( model, Cmd.none )

        TaskReceived (Ok task) ->
            ( { model | todos = Tasks.insertTask task model.todos }, Cmd.none )

        TaskReceived (Err _) ->
            -- for this Demo we ignore communication errors - sorry
            ( model, Cmd.none )

        ListMetaDataReceived (Ok metaData) ->
            ( { model | taskList = RemoteData.Success metaData }, Cmd.none )

        ListMetaDataReceived (Err httpError) ->
            case httpError of
                Http.BadStatus 401 ->
                    ( model, Session.navigateTo model Routes.Login )

                _ ->
                    ( { model | taskList = RemoteData.Failure httpError }, Cmd.none )


view : Model mainMsg -> Html mainMsg
view model =
    viewTodoApp model
        |> H.map model.map


viewTodoApp : Model mainMsg -> Html Msg
viewTodoApp model =
    H.section
        [ Attr.class "todoapp" ]
        [ viewHeader model
        , viewMain model
        , viewFooter model
        ]


viewHeader : Model mainMsg -> Html Msg
viewHeader model =
    H.header
        [ Attr.class "header" ]
        [ H.h1 [] [ H.text (model.taskList |> RemoteData.map .name |> RemoteData.withDefault "") ]
        , H.input
            [ Attr.class "new-todo"
            , Attr.placeholder "what needs to be done?"
            , Attr.autofocus True
            , Attr.value model.newText
            , Ev.onInput UpdateNewText
            , onFinish AddTodo ClearNewText
            , Ev.onBlur ClearNewText
            ]
            []
        ]


{-| should be hidden by default and shown when there are todos
-}
viewMain : Model mainMsg -> Html Msg
viewMain model =
    if Tasks.isEmpty model.todos then
        H.text ""

    else
        H.section
            [ Attr.class "main" ]
            [ H.input
                [ Attr.id "toggle-all"
                , Attr.class "toggle-all"
                , Attr.type_ "checkbox"
                , Attr.checked (Tasks.allCompleted model.todos)
                , Ev.onCheck ToggleAll
                ]
                []
            , H.label
                [ Attr.for "toggle-all" ]
                [ H.text "Mark all as complete" ]

            -- li-tasks with class completed (if it is), containing viewTask or editTask
            , H.ul
                [ Attr.class "todo-list" ]
                (filtered model.activeFilter model.todos |> List.map (viewTask model.editingTask))
            ]


{-| should display an task
-}
viewTask : Maybe { id : Task.Id, text : String } -> Task -> Html Msg
viewTask editing task =
    let
        isEditing =
            Maybe.map .id editing == Just task.id
    in
    H.li
        [ Attr.classList
            [ ( "completed", task.completed )
            , ( "editing", isEditing )
            ]
        ]
        [ H.div
            [ Attr.class "view" ]
            [ H.input
                [ Attr.class "toggle"
                , Attr.type_ "checkbox"
                , Attr.checked task.completed
                , Ev.onCheck (CheckTask task.id)
                ]
                []
            , H.label
                [ Ev.onDoubleClick (EditTask task)
                ]
                [ H.text task.text ]
            , H.button
                [ Attr.class "destroy"
                , Ev.onClick (DeleteTask task.id)
                ]
                []
            ]
        , H.input
            [ Attr.class "edit"
            , Attr.id ("edit_" ++ Task.idToString task.id)
            , Attr.value (Maybe.map .text editing |> Maybe.withDefault "")
            , Ev.onInput UpdateEditText
            , onFinish FinishEdit CancelEdit
            , Ev.onBlur CancelEdit
            ]
            []
        ]


{-| should be hidden by default and shown when there are todos
-}
viewFooter : Model mainMsg -> Html Msg
viewFooter model =
    if Tasks.isEmpty model.todos then
        H.text ""

    else
        H.footer
            [ Attr.class "footer" ]
            [ H.span [ Attr.class "todo-count" ]
                [ H.strong [] [ H.text (String.fromInt (model.todos |> activeCount)) ]
                , H.text " task left"
                ]
            , H.ul [ Attr.class "filters" ]
                [ H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == All ) ]
                        , Attr.href (Routes.routeToUrlString model.session.flags.baseUrlPath (Routes.List model.listId All))
                        ]
                        [ H.text "All" ]
                    ]
                , H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == Active ) ]
                        , Attr.href (Routes.routeToUrlString model.session.flags.baseUrlPath (Routes.List model.listId Active))
                        ]
                        [ H.text "Active" ]
                    ]
                , H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == Completed ) ]
                        , Attr.href (Routes.routeToUrlString model.session.flags.baseUrlPath (Routes.List model.listId Completed))
                        ]
                        [ H.text "Completed" ]
                    ]
                ]
            , H.button
                [ Attr.class "clear-completed"
                , Ev.onClick ClearCompleted
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
