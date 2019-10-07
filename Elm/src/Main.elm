module Main exposing (main)

import Auth
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Flags exposing (Flags)
import LocalStorage
import Navigation.Routes as Routes exposing (Route)
import Page.Login as LoginPage
import Page.LoginPending as LoginPending
import Page.TaskList as TaskListPage
import Page.TaskLists as TaskListsPage
import Session exposing (Session)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags location key =
    let
        ( session, sessionCmd ) =
            Session.init flags key
    in
    let
        ( model, cmd ) =
            case Routes.locationToRoute flags.baseUrlPath location of
                Just route ->
                    initPage session route

                Nothing ->
                    let
                        ( initModel, initCmd ) =
                            initPage session Routes.Lists
                    in
                    ( initModel
                    , Cmd.batch
                        [ Nav.replaceUrl key (Routes.routeToUrlString flags.baseUrlPath Routes.Lists)
                        , initCmd
                        ]
                    )
    in
    ( model
    , Cmd.batch
        [ cmd
        , sessionCmd
        ]
    )


type Model
    = Login (LoginPage.Model Msg)
    | LoginPending (LoginPending.Model Msg)
    | List (TaskListPage.Model Msg)
    | Lists (TaskListsPage.Model Msg)


type Msg
    = NoOp
    | UrlRequested UrlRequest
    | UrlChanged Url
    | AuthorizationStoreChanged (Session -> Session)
    | LoginMsg LoginPage.Msg
    | LoginPendingMsg LoginPending.Msg
    | ListMsg TaskListPage.Msg
    | ListsMsg TaskListsPage.Msg


initPage : Session -> Route -> ( Model, Cmd Msg )
initPage session route =
    if not (Auth.isAuthenticated session) && route /= Routes.Login then
        let
            ( pageModel, pageCmd ) =
                LoginPending.init LoginPendingMsg session (Just route)
        in
        ( LoginPending pageModel, pageCmd )

    else
        case route of
            Routes.Login ->
                let
                    ( pageModel, pageCmd ) =
                        LoginPage.init LoginMsg session Nothing
                in
                ( Login pageModel, pageCmd )

            Routes.List listId filter ->
                let
                    ( pageModel, pageCmd ) =
                        TaskListPage.init ListMsg session filter listId
                in
                ( List pageModel, pageCmd )

            Routes.Lists ->
                let
                    ( pageModel, pageCmd ) =
                        TaskListsPage.init ListsMsg session
                in
                ( Lists pageModel, pageCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AuthorizationStoreChanged updSession ->
            let
                newModel =
                    updateSession updSession model

                navCmd =
                    if withSession Auth.isAuthenticated newModel then
                        Cmd.none

                    else
                        navigateTo model Routes.Login
            in
            ( newModel, navCmd )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case locationToRoute model url of
                        Nothing ->
                            ( model, navigateTo model Routes.Lists )

                        Just route ->
                            ( model, navigateTo model route )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            if not (withSession Auth.isAuthenticated model) then
                initPage (getSession model) Routes.Login

            else
                let
                    route =
                        locationToRoute model url
                in
                case route of
                    Just r ->
                        initPage (getSession model) r

                    Nothing ->
                        let
                            ( page, pageCmd ) =
                                initPage (getSession model) Routes.Lists
                        in
                        ( page, Cmd.batch [ pageCmd, replaceUrl model Routes.Lists ] )

        ListMsg pageMsg ->
            updateList pageMsg model

        LoginMsg pageMsg ->
            updateLogin pageMsg model

        ListsMsg pageMsg ->
            updateLists pageMsg model

        LoginPendingMsg pageMsg ->
            updateLoginPending pageMsg model


updateLogin : LoginPage.Msg -> Model -> ( Model, Cmd Msg )
updateLogin msg pageModel =
    case pageModel of
        Login loginModel ->
            let
                ( newLoginModel, cmd ) =
                    LoginPage.update msg loginModel
            in
            ( Login newLoginModel, cmd )

        _ ->
            ( pageModel, Cmd.none )


updateLoginPending : LoginPending.Msg -> Model -> ( Model, Cmd Msg )
updateLoginPending msg pageModel =
    case pageModel of
        LoginPending loginPendingModel ->
            let
                ( newLoginPendingModel, cmd ) =
                    LoginPending.update msg loginPendingModel
            in
            ( LoginPending newLoginPendingModel, cmd )

        _ ->
            ( pageModel, Cmd.none )


updateList : TaskListPage.Msg -> Model -> ( Model, Cmd Msg )
updateList msg pageModel =
    case pageModel of
        List listModel ->
            let
                ( newListModel, cmd ) =
                    TaskListPage.update msg listModel
            in
            ( List newListModel, cmd )

        _ ->
            ( pageModel, Cmd.none )


updateLists : TaskListsPage.Msg -> Model -> ( Model, Cmd Msg )
updateLists msg pageModel =
    case pageModel of
        Lists listsModel ->
            let
                ( newListsModel, cmd ) =
                    TaskListsPage.update msg listsModel
            in
            ( Lists newListsModel, cmd )

        _ ->
            ( pageModel, Cmd.none )


view : Model -> Document Msg
view model =
    let
        page =
            case model of
                List listModel ->
                    TaskListPage.view listModel

                Login loginModel ->
                    LoginPage.view loginModel

                Lists listsModel ->
                    TaskListsPage.view listsModel

                LoginPending loginPendingModel ->
                    LoginPending.view loginPendingModel
    in
    { title = "TODO - Elm"
    , body = [ page ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubs =
            case model of
                List listModel ->
                    TaskListPage.subscriptions listModel

                Login loginModel ->
                    LoginPage.subscriptions loginModel

                Lists listsModel ->
                    TaskListsPage.subscriptions listsModel

                LoginPending loginPendingModel ->
                    LoginPending.subscriptions loginPendingModel
    in
    Sub.batch [ pageSubs, Auth.watchLocalStorage NoOp AuthorizationStoreChanged ]


updateSession : (Session -> Session) -> Model -> Model
updateSession upd model =
    case model of
        List page ->
            List { page | session = upd page.session }

        Login page ->
            Login { page | session = upd page.session }

        Lists page ->
            Lists { page | session = upd page.session }

        LoginPending page ->
            LoginPending { page | session = upd page.session }


withSession : (Session -> a) -> Model -> a
withSession with model =
    case model of
        List page ->
            with page.session

        Login page ->
            with page.session

        Lists page ->
            with page.session

        LoginPending page ->
            with page.session


getSession : Model -> Session
getSession =
    withSession identity


locationToRoute : Model -> Url -> Maybe Route
locationToRoute model url =
    withSession (\session -> Routes.locationToRoute session.flags.baseUrlPath url) model


navigateTo : Model -> Route -> Cmd msg
navigateTo model route =
    withSession (\session -> Routes.navigateTo session route) model


replaceUrl : Model -> Route -> Cmd msg
replaceUrl model route =
    withSession (\session -> Routes.replaceUrl session route) model
