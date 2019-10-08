module Flags exposing (Flags)

type alias BaseUrlPath = String

type alias Flags =
    { baseUrlPath : BaseUrlPath
    , apiUrl : BaseUrlPath
    }
