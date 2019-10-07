module Session exposing
    ( Session
    , init
    , navigateTo
    , replaceUrl
    )

import AppUrl
import Auth
import Browser.Navigation as Nav
import Flags exposing (Flags)
import Http
import Routes
import String.Interpolate as String
import Url.Builder as Url


type alias Session =
    { flags : Flags
    , navKey : Nav.Key
    , authentication : Auth.Authentication
    }


init : Flags -> Nav.Key -> ( Session, Cmd msg )
init flags key =
    ( Session flags key Auth.init
    , Cmd.none
    )


navigateTo : Session -> Routes.Route -> Cmd msg
navigateTo session route =
    let
        url =
            getRouteUrl session route

        key =
            session.navKey
    in
    Nav.pushUrl key url


replaceUrl : Session -> Routes.Route -> Cmd msg
replaceUrl session route =
    let
        url =
            getRouteUrl session route

        key =
            session.navKey
    in
    Nav.replaceUrl key url


getRouteUrl : Session -> Routes.Route -> String
getRouteUrl session route =
    Routes.routeToUrlString session.flags.baseUrlPath route
