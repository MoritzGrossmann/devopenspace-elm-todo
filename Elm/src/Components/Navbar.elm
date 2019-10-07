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
    case Auth.getUserName session of
        Just userName ->
            H.div [ Attr.class "navbar" ]
                [ H.h2 [] [ H.text ("Hello " ++ userName) ]
                , H.span [ Attr.class "logout-btn" ]
                    [ H.a [ Attr.href (Routes.routeToUrlString session.flags.baseUrlPath Routes.Login) ]
                        [ H.text "logout" ]
                    ]
                ]

        Nothing ->
            H.div [ Attr.class "navbar" ]
                [ H.h2 [] [ H.text "please " ]
                , H.span [ Attr.class "logout-btn" ]
                    [ H.a [ Attr.href (Routes.routeToUrlString session.flags.baseUrlPath Routes.Login) ]
                        [ H.text "log in" ]
                    ]
                ]
