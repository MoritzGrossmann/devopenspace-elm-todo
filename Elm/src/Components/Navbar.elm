module Components.Navbar exposing (view)

import Auth exposing (AuthToken(..))
import Flags
import Html as H exposing (Html)
import Html.Attributes as Attr
import Jwt
import Navigation.Routes as Routes
import Session exposing (Session)


view : Session -> Html msg
view session =
    let
        name =
            case session.authentication of
                Auth.Authenticated (AuthToken token) ->
                    let
                        res =
                            Jwt.decodeToken Auth.jwtDecoder token
                    in
                    case res of
                        Ok jwt ->
                            jwt.dat.auName

                        Err error ->
                            Jwt.errorToString error

                _ ->
                    ""
    in
    H.div [ Attr.class "navbar" ]
        [ H.h2 [] [ H.text ("Hallo " ++ name) ]
        , H.span [ Attr.class "logout-btn" ]
            [ H.a [ Attr.href (Routes.routeToUrlString session.flags.baseUrlPath Routes.Login) ]
                [ H.text "logout" ]
            ]
        ]
