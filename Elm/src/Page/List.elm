module Page.List exposing (Model, Msg, Page, init, update, view)

import Api.Todos as Api
import Browser.Dom as Dom
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Decode as Json
import Models.List as List
import Page
import Session exposing (Session)
import Task
import Todos exposing (Filter(..), Todos)


type alias Page msg =
    Page.Page msg Model Msg


type alias Model =
    { todos : Todos
    , newText : String
    , editingItem : Maybe { id : Todos.Id, text : String }
    , activeFilter : Filter
    , session : Session
    , listMetaData : Maybe List.MetaData
    }


type Msg
    = NoOp
    | UpdateNewText String
    | ClearNewText
    | AddTodo
    | CheckItem Todos.Id Bool
    | DeleteItem Todos.Id
    | EditItem Todos.Item
    | UpdateEditText String
    | FinishEdit
    | CancelEdit
    | ClearCompleted
    | ToggleAll Bool
    | ItemsReceived (Result Http.Error (List Todos.Item))
    | ItemReceived (Result Http.Error Todos.Item)


init : (Page.PageMsg Msg -> msg) -> Session -> Filter -> Int -> ( Page msg, Cmd msg )
init wrap session filter listId =
    let
        pageInit _ =
            ( { todos = Todos.empty
              , newText = ""
              , editingItem = Nothing
              , activeFilter = filter
              , session = session
              , listMetaData = Nothing
              }
            , Cmd.batch
                [ Api.getAll session.flags.baseUrlPath ItemsReceived
                ]
            )
    in
    Page.init wrap pageInit view update (always Sub.none) session


itemCount : Model -> Int
itemCount model =
    List.length (filtered model.activeFilter model.todos)


filtered : Filter -> Todos -> List Todos.Item
filtered filter =
    case filter of
        All ->
            Todos.allTodos

        Active ->
            Todos.activeTodos

        Completed ->
            Todos.completedTodos


update : Msg -> Model -> ( Model, Cmd Msg )
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
                    Api.new model.session.flags.baseUrlPath ItemReceived model.newText
            in
            ( { model | newText = "" }, insertCmd )

        CheckItem itemId completed ->
            let
                ( newItem, newTodos ) =
                    Todos.setCompleted completed itemId model.todos

                updateCmd =
                    newItem
                        |> Maybe.map (Api.update model.session.flags.baseUrlPath ItemReceived)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | todos = newTodos }, updateCmd )

        DeleteItem itemId ->
            let
                deleteCmd =
                    Api.delete model.session.flags.baseUrlPath ItemsReceived itemId

                newTodos =
                    Todos.deleteItem itemId model.todos
            in
            ( { model | todos = newTodos }, deleteCmd )

        EditItem item ->
            let
                edit =
                    { id = item.id
                    , text = item.text
                    }
            in
            ( { model | editingItem = Just edit }
            , Task.attempt (always NoOp) (Dom.focus ("edit_" ++ Todos.idToString item.id))
            )

        UpdateEditText updatedText ->
            let
                edit =
                    model.editingItem
                        |> Maybe.map (\item -> { item | text = updatedText })
            in
            ( { model | editingItem = edit }, Cmd.none )

        CancelEdit ->
            ( { model | editingItem = Nothing }, Cmd.none )

        FinishEdit ->
            let
                ( updatedItem, newTodos ) =
                    model.editingItem
                        |> Maybe.map
                            (\editingItem ->
                                Todos.setText editingItem.text editingItem.id model.todos
                            )
                        |> Maybe.withDefault ( Nothing, model.todos )

                updateCmd =
                    updatedItem
                        |> Maybe.map (Api.update model.session.flags.baseUrlPath ItemReceived)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model
                | todos = newTodos
                , editingItem = Nothing
              }
            , updateCmd
            )

        ClearCompleted ->
            let
                delete todos id =
                    Todos.deleteItem id todos

                ( deleteCmds, deletedTodos ) =
                    Todos.completedTodos model.todos
                        |> List.foldl
                            (\item ( cmds, todos ) ->
                                let
                                    newTodos =
                                        Todos.deleteItem item.id model.todos

                                    deleteCmd =
                                        Api.delete model.session.flags.baseUrlPath ItemsReceived item.id
                                in
                                ( deleteCmd :: cmds, newTodos )
                            )
                            ( [], model.todos )
            in
            ( { model | todos = deletedTodos }, Cmd.batch deleteCmds )

        ToggleAll setCompleted ->
            let
                toggle todos id =
                    Todos.setCompleted setCompleted id todos

                ( toggleCmds, toggledTodos ) =
                    filtered model.activeFilter model.todos
                        |> List.foldl
                            (\item ( cmds, todos ) ->
                                let
                                    ( toggledItem, newTodos ) =
                                        toggle todos item.id

                                    toggleCmd =
                                        toggledItem
                                            |> Maybe.map (Api.update model.session.flags.baseUrlPath ItemReceived)
                                            |> Maybe.withDefault Cmd.none
                                in
                                ( toggleCmd :: cmds, newTodos )
                            )
                            ( [], model.todos )
            in
            ( { model | todos = toggledTodos }, Cmd.batch toggleCmds )

        ItemsReceived (Ok items) ->
            ( { model | todos = Todos.fromList items }, Cmd.none )

        ItemsReceived (Err _) ->
            -- for this Demo we ignore communication errors - sorry
            ( model, Cmd.none )

        ItemReceived (Ok item) ->
            ( { model | todos = Todos.insertItem item model.todos }, Cmd.none )

        ItemReceived (Err _) ->
            -- for this Demo we ignore communication errors - sorry
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    viewTodoApp model


viewTodoApp : Model -> Html Msg
viewTodoApp model =
    H.section
        [ Attr.class "todoapp" ]
        [ viewHeader model
        , viewMain model
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    H.header
        [ Attr.class "header" ]
        [ H.h1 [] [ H.text "todos" ]
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
viewMain : Model -> Html Msg
viewMain model =
    if Todos.isEmpty model.todos then
        H.text ""

    else
        H.section
            [ Attr.class "main" ]
            [ H.input
                [ Attr.id "toggle-all"
                , Attr.class "toggle-all"
                , Attr.type_ "checkbox"
                , Attr.checked (Todos.allCompleted model.todos)
                , Ev.onCheck ToggleAll
                ]
                []
            , H.label
                [ Attr.for "toggle-all" ]
                [ H.text "Mark all as complete" ]

            -- li-items with class completed (if it is), containing viewItem or editItem
            , H.ul
                [ Attr.class "todo-list" ]
                (filtered model.activeFilter model.todos |> List.map (viewItem model.editingItem))
            ]


{-| should display an item
-}
viewItem : Maybe { id : Todos.Id, text : String } -> Todos.Item -> Html Msg
viewItem editing item =
    let
        isEditing =
            Maybe.map .id editing == Just item.id
    in
    H.li
        [ Attr.classList
            [ ( "completed", item.completed )
            , ( "editing", isEditing )
            ]
        ]
        [ H.div
            [ Attr.class "view" ]
            [ H.input
                [ Attr.class "toggle"
                , Attr.type_ "checkbox"
                , Attr.checked item.completed
                , Ev.onCheck (CheckItem item.id)
                ]
                []
            , H.label
                [ Ev.onDoubleClick (EditItem item)
                ]
                [ H.text item.text ]
            , H.button
                [ Attr.class "destroy"
                , Ev.onClick (DeleteItem item.id)
                ]
                []
            ]
        , H.input
            [ Attr.class "edit"
            , Attr.id ("edit_" ++ Todos.idToString item.id)
            , Attr.value (Maybe.map .text editing |> Maybe.withDefault "")
            , Ev.onInput UpdateEditText
            , onFinish FinishEdit CancelEdit
            , Ev.onBlur CancelEdit
            ]
            []
        ]


{-| should be hidden by default and shown when there are todos
-}
viewFooter : Model -> Html Msg
viewFooter model =
    if Todos.isEmpty model.todos then
        H.text ""

    else
        H.footer
            [ Attr.class "footer" ]
            [ H.span [ Attr.class "todo-count" ]
                [ H.strong [] [ H.text (String.fromInt (itemCount model)) ]
                , H.text " item left"
                ]
            , H.ul [ Attr.class "filters" ]
                [ H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == All ) ]
                        , Attr.href ""
                        ]
                        [ H.text "All" ]
                    ]
                , H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == Active ) ]
                        , Attr.href ""
                        ]
                        [ H.text "Active" ]
                    ]
                , H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == Completed ) ]
                        , Attr.href ""
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
