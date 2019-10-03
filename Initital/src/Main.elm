module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Flags exposing (Flags)
import Page
import Page.Login as LoginPage
import Page.LoginPending as LoginPending
import Routes exposing (Route)
import Session exposing (Login(..), Session, getNavKey)
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
                            initPage session Routes.Login
                    in
                    ( initModel
                    , Cmd.batch
                        [ Nav.replaceUrl key (Routes.routeToUrlString flags.baseUrlPath Routes.Login)
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
    = Login (LoginPage.Page Msg)
    | LoginPending (LoginPending.Page Msg)


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | LoginMsg (Page.PageMsg LoginPage.Msg)
    | LoginPendingMsg (Page.PageMsg LoginPending.Msg)


initPage : Session -> Route -> ( Model, Cmd Msg )
initPage session route =
    if session.login == NotLoggedIn && route /= Routes.Login then
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Routes.locationToRoute (model |> getFlags).baseUrlPath url of
                        Nothing ->
                            ( model, Nav.pushUrl (getNavKey model) (Routes.routeToUrlString (getFlags model).baseUrlPath Routes.Login) )

                        Just route ->
                            ( model, Nav.pushUrl (getNavKey model) (Routes.routeToUrlString (getFlags model).baseUrlPath route) )

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
                            initPage (withSession identity model) Routes.Login

        LoginMsg pageMsg ->
            updateLogin pageMsg model

        LoginPendingMsg pageMsg ->
            updateLoginPending pageMsg model


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


updateLoginPending : Page.PageMsg LoginPending.Msg -> Model -> ( Model, Cmd Msg )
updateLoginPending msg pageModel =
    case pageModel of
        LoginPending loginPendingModel ->
            let
                ( newLoginPendingModel, cmd ) =
                    Page.update msg loginPendingModel
            in
            ( LoginPending newLoginPendingModel, cmd )

        _ ->
            ( pageModel, Cmd.none )


view : Model -> Document Msg
view model =
    let
        page =
            case model of
                Login loginModel ->
                    Page.view loginModel

                LoginPending loginPendingModel ->
                    Page.view loginPendingModel
    in
    { title = "TODO - Elm"
    , body = [ page ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Login loginModel ->
            Page.subscriptions loginModel

        LoginPending loginPendingModel ->
            Page.subscriptions loginPendingModel


getFlags : Model -> Flags
getFlags =
    withSession Session.getFlags


getNavKey : Model -> Nav.Key
getNavKey =
    withSession Session.getNavKey


withSession : (Session -> a) -> Model -> a
withSession with model =
    case model of
        Login page ->
            with (Page.getSession page)

        LoginPending page ->
            with (Page.getSession page)
