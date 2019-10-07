module Flags exposing (Flags)

import Navigation.AppUrl exposing (BaseUrlPath)


type alias Flags =
    { baseUrlPath : BaseUrlPath
    , apiUrl : BaseUrlPath
    }
