module Session exposing
    ( Session
    , init
    )

import Browser.Navigation as Nav
import Flags exposing (Flags)


type alias Session =
    { flags : Flags
    }


init : Flags -> ( Session, Cmd msg )
init flags =
    ( Session flags
    , Cmd.none
    )
