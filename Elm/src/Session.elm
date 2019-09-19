module Session exposing
    ( Session
    , getFlags
    , getNavKey
    , getUrl
    , init
    , makeSessionUrl
    , navigateTo
    , replaceUrl
    , routeUrl
    , sessionRouteUrl
    )

import Browser.Navigation as Nav
import Flags exposing (Flags)
import Models.User as User
import Routes
import Url.Builder as Url


type alias Session =
    { flags : Flags
    , navKey : Nav.Key
    , user : Maybe User.Info
    }


init : Flags -> Nav.Key -> ( Session, Cmd msg )
init flags key =
    ( Session flags key Nothing
    , Cmd.none
    )


makeSessionUrl : Session -> List String -> List Url.QueryParameter -> Maybe String -> String
makeSessionUrl session =
    Flags.makeUrl session.flags


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
