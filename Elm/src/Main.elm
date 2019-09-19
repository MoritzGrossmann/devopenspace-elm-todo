module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Flags exposing (Flags)
import Page
import Page.List as ListPage
import Page.Login as LoginPage
import Routes exposing (Route)
import Session exposing (Login(..), Session, getNavKey)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags location key =
    let
        ( session, sessionCmd ) =
            Session.init flags key
    in
    case Routes.locationToRoute flags.baseUrlPath location of
        Just route ->
            initPage session route

        Nothing ->
            let
                ( initModel, initCmd ) =
                    initPage session Routes.Login
            in
            ( initModel
            , Cmd.batch
                [ Nav.replaceUrl key (Routes.routeToUrlString flags.baseUrlPath Routes.Login)
                , initCmd
                , sessionCmd
                ]
            )


type Model
    = Login (LoginPage.Page Msg)
    | List (ListPage.Page Msg)


type Msg
    = NoOp
    | UrlRequested UrlRequest
    | UrlChanged Url
    | ListMsg (Page.PageMsg ListPage.Msg)
    | LoginMsg (Page.PageMsg LoginPage.Msg)


initPage : Session -> Route -> ( Model, Cmd Msg )
initPage session route =
    case route of
        Routes.Login ->
            let
                ( pageModel, pageCmd ) =
                    LoginPage.init LoginMsg session
            in
            ( Login pageModel, pageCmd )

        Routes.List listId filter ->
            let
                ( pageModel, pageCmd ) =
                    ListPage.init ListMsg session filter listId
            in
            ( List pageModel, pageCmd )

        Routes.Lists ->
            let
                ( pageModel, pageCmd ) =
                    LoginPage.init LoginMsg session
            in
            ( Login pageModel, pageCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Routes.locationToRoute (model |> getFlags).baseUrlPath url of
                        Nothing ->
                            ( model, Nav.replaceUrl (getNavKey model) (Routes.routeToUrlString (getFlags model).baseUrlPath Routes.Lists) )

                        Just route ->
                            let
                                ( page, cmd ) =
                                    initPage (withSession identity model) route
                            in
                            ( page, cmd )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            case (withSession identity model).login of
                NotLoggedIn ->
                    initPage (withSession identity model) Routes.Login

                LoggedIn _ ->
                    let
                        route =
                            Routes.locationToRoute (getFlags model).baseUrlPath url
                    in
                    case route of
                        Just r ->
                            initPage (withSession identity model) r

                        Nothing ->
                            initPage (withSession identity model) Routes.Lists

        ListMsg pageMsg ->
            updateList pageMsg model

        LoginMsg pageMsg ->
            updateLogin pageMsg model


updateLogin : Page.PageMsg LoginPage.Msg -> Model -> ( Model, Cmd Msg )
updateLogin msg pageModel =
    case pageModel of
        Login loginModel ->
            let
                ( newLoginModel, cmd ) =
                    Page.update msg loginModel
            in
            ( Login newLoginModel, cmd )

        _ ->
            ( pageModel, Cmd.none )


updateList : Page.PageMsg ListPage.Msg -> Model -> ( Model, Cmd Msg )
updateList msg pageModel =
    case pageModel of
        List listModel ->
            let
                ( newListModel, cmd ) =
                    Page.update msg listModel
            in
            ( List newListModel, cmd )

        _ ->
            ( pageModel, Cmd.none )


view : Model -> Document Msg
view model =
    let
        page =
            case model of
                List listModel ->
                    Page.view listModel

                Login loginModel ->
                    Page.view loginModel
    in
    { title = "TODO - Elm"
    , body = [ page ]
    }


getFlags : Model -> Flags
getFlags =
    withSession Session.getFlags


getNavKey : Model -> Nav.Key
getNavKey =
    withSession Session.getNavKey


withSession : (Session -> a) -> Model -> a
withSession with model =
    case model of
        List page ->
            with (Page.getSession page)

        Login page ->
            with (Page.getSession page)
