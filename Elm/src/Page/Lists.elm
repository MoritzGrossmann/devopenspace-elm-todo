module Page.Lists exposing (Model, Msg(..), Page, init, update, view)

import Api.Lists
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Models.TaskList as TaskList exposing (TaskList)
import Models.Tasks exposing (Filter(..))
import Page
import RemoteData exposing (WebData)
import Routes
import Session exposing (Session)


type alias Page msg =
    Page.Page msg Model Msg


type alias Model =
    { session : Session
    , lists : WebData (Dict Int TaskList)
    , neueListe : String
    }


init : (Page.PageMsg Msg -> msg) -> Session -> ( Page msg, Cmd msg )
init wrap session =
    let
        pageInit _ =
            ( { session = session
              , lists = RemoteData.Loading
              , neueListe = ""
              }
            , Api.Lists.all session ListResult
            )
    in
    Page.init wrap pageInit view update (always Sub.none) session


type Msg
    = ListResult (Result Http.Error (List TaskList))
    | UpdateNeueListe String
    | SubmitNeueListe
    | NeueListeResult (Result Http.Error TaskList)
    | DeleteList TaskList.Id
    | DeleteListResult TaskList.Id (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ListResult (Ok lists) ->
            ( { model | lists = RemoteData.Success (lists |> listToDict) }, Cmd.none )

        ListResult (Err httpError) ->
            case httpError of
                Http.BadStatus 401 ->
                    ( { model | lists = RemoteData.Failure httpError }, Session.navigateTo model Routes.Login )

                _ ->
                    ( { model | lists = RemoteData.Failure httpError }, Cmd.none )

        UpdateNeueListe string ->
            ( { model | neueListe = string }, Cmd.none )

        SubmitNeueListe ->
            ( model, Api.Lists.add model.session NeueListeResult model.neueListe )

        NeueListeResult (Ok liste) ->
            ( { model
                | neueListe = ""
                , lists = model.lists |> RemoteData.map (\listen -> RemoteData.Success (listen |> Dict.insert (TaskList.idToInt liste.id) liste)) |> RemoteData.withDefault model.lists
              }
            , Cmd.none
            )

        NeueListeResult (Err _) ->
            ( model, Cmd.none )

        DeleteList id ->
            ( model, Api.Lists.delete model.session (DeleteListResult id) id )

        DeleteListResult id (Ok _) ->
            ( { model | lists = model.lists |> RemoteData.map (Dict.remove (TaskList.idToInt id)) }, Cmd.none )

        DeleteListResult _ (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
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
                    (\dict -> dict |> Dict.toList |> List.map (\( _, m ) -> viewListItem model.session m))
                |> RemoteData.withDefault []
            )
        ]


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
