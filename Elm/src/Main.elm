module Main exposing (main)

import Auth
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Flags exposing (Flags)
import Navigation.Routes as Routes exposing (Route)
import Page.Login as LoginPage
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
                        -- TODO nach Lists navigieren
                        ( initModel, initCmd ) =
                            initPage session Routes.Login
                    in
                    ( initModel
                    , Cmd.batch
                        -- TODO mit Lists erzetzen
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
    = Login (LoginPage.Model Msg)


type Msg
    = NoOp
    | UrlRequested UrlRequest
    | UrlChanged Url
    | LoginMsg LoginPage.Msg


initPage : Session -> Route -> ( Model, Cmd Msg )
initPage session route =
    if not (Auth.isAuthenticated session) && route /= Routes.Login then
        let
            ( pageModel, pageCmd ) =
                LoginPage.init LoginMsg session (Just route)
        in
        ( Login pageModel, pageCmd )

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
        NoOp ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case locationToRoute model url of
                        Nothing ->
                            -- TODO nach Lists navigieren
                            ( model, navigateTo model Routes.Login )

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
                                -- TODO zu Lists
                                initPage (getSession model) Routes.Login
                        in
                        -- TODO zu Lists
                        ( page, Cmd.batch [ pageCmd, replaceUrl model Routes.Login ] )

        LoginMsg pageMsg ->
            updateLogin pageMsg model


updateLogin : LoginPage.Msg -> Model -> ( Model, Cmd Msg )
updateLogin msg pageModel =
    case pageModel of
        Login loginModel ->
            let
                ( newLoginModel, cmd ) =
                    LoginPage.update msg loginModel
            in
            ( Login newLoginModel, cmd )


view : Model -> Document Msg
view model =
    let
        page =
            case model of
                Login loginModel ->
                    LoginPage.view loginModel
    in
    { title = "TODO - Elm"
    , body = [ page ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Login loginModel ->
            LoginPage.subscriptions loginModel


updateSession : (Session -> Session) -> Model -> Model
updateSession upd model =
    case model of
        Login page ->
            Login { page | session = upd page.session }


withSession : (Session -> a) -> Model -> a
withSession with model =
    case model of
        Login page ->
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
