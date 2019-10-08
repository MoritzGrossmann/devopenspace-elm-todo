module Session exposing
    ( Session
    , init
    )

import Auth
import Browser.Navigation as Nav
import Flags exposing (Flags)


type alias Session =
    { flags : Flags
    , authentication : Auth.Authentication
    }


init : Flags -> ( Session, Cmd msg )
init flags =
    ( Session flags Auth.init
    , Cmd.none
    )
