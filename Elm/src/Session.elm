module Session exposing
    ( Session
    , init
    )

import Auth
import Browser.Navigation as Nav
import Flags exposing (Flags)


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
