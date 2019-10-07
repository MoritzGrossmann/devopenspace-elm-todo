module Components.Navbar exposing (view)

import Auth
import Flags
import Html as H exposing (Html)
import Html.Attributes as Attr
import Jwt
import Navigation.Routes as Routes
import Session exposing (Session)


view : Session -> Html msg
view session =
    H.div [ Attr.class "navbar" ]
        [ H.h2 [] [ H.text ("Hallo " ++ (Auth.getUserName session)) ]
        , H.span [ Attr.class "logout-btn" ]
            [ H.a [ Attr.href (Routes.routeToUrlString session.flags.baseUrlPath Routes.Login) ]
                [ H.text "logout" ]
            ]
        ]
