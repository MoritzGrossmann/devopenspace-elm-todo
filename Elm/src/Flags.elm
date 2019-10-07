module Flags exposing (Flags)

import AppUrl exposing (BaseUrlPath)


type alias Flags =
    { baseUrlPath : BaseUrlPath
    , apiUrl : BaseUrlPath
    }
