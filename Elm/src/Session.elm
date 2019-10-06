module Session exposing
    ( Session
    , getFlags
    , getNavKey
    , getUrl
    , init
    , makeApiUrl
    , makeSessionUrl
    , navigateTo
    , replaceUrl
    , routeUrl
    , sessionRouteUrl
    )

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


makeSessionUrl : Session -> List String -> List Url.QueryParameter -> Maybe String -> String
makeSessionUrl session =
    Flags.makeUrl session.flags


makeApiUrl : Session -> List String -> List Url.QueryParameter -> Maybe String -> String
makeApiUrl session =
    Flags.makeApiUrl session.flags


getUrl : { m | session : Session } -> List String -> List Url.QueryParameter -> Maybe String -> String
getUrl model =
    makeSessionUrl model.session


getFlags : Session -> Flags
getFlags session =
    session.flags


getNavKey : Session -> Nav.Key
getNavKey session =
    session.navKey


navigateTo : { m | session : Session } -> Routes.Route -> Cmd msg
navigateTo model route =
    let
        url =
            routeUrl model route

        key =
            model.session.navKey
    in
    Nav.pushUrl key url


replaceUrl : { m | session : Session } -> Routes.Route -> Cmd msg
replaceUrl model route =
    let
        url =
            routeUrl model route

        key =
            model.session.navKey
    in
    Nav.replaceUrl key url


routeUrl : { m | session : Session } -> Routes.Route -> String
routeUrl model =
    sessionRouteUrl model.session


sessionRouteUrl : Session -> Routes.Route -> String
sessionRouteUrl session route =
    Routes.routeToUrlString session.flags.baseUrlPath route
