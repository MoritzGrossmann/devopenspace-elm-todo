module Page exposing (Page, PageMsg, getSession, init, subscriptions, update, view)

import Html as H exposing (Html)
import Session exposing (Session)


type alias Page mainMsg model msg =
    { view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , wrapMsg : PageMsg msg -> mainMsg
    , model : model
    }


type PageMsg msg
    = PageMsg msg


getSession : Page mainMsg { model | session : Session } msg -> Session
getSession page =
    page.model.session


init : (PageMsg msg -> mainMsg) -> (Session -> ( model, Cmd msg )) -> (model -> Html msg) -> (msg -> model -> ( model, Cmd msg )) -> (model -> Sub msg) -> Session -> ( Page mainMsg model msg, Cmd mainMsg )
init wrapMsg initPageModel viewPage updatePage subPage session =
    let
        ( model, pageCmd ) =
            initPageModel session
    in
    ( { view = viewPage
      , update = updatePage
      , subscriptions = subPage
      , wrapMsg = wrapMsg
      , model = model
      }
    , Cmd.batch [ Cmd.map (PageMsg >> wrapMsg) pageCmd ]
    )


view : Page msg pageModel pageMsg -> Html msg
view page =
    page.view page.model |> H.map (PageMsg >> page.wrapMsg)


update : PageMsg pageMsg -> Page msg { pageModel | session : Session } pageMsg -> ( Page msg { pageModel | session : Session } pageMsg, Cmd msg )
update pageMsg page =
    case pageMsg of
        PageMsg msg ->
            let
                ( newSubModel, cmd ) =
                    page.update msg page.model
            in
            ( { page | model = newSubModel }, Cmd.map (PageMsg >> page.wrapMsg) cmd )



----------------------------------------------------------------------------
-- Subscription


subscriptions : Page mainMsg model msg -> Sub mainMsg
subscriptions page =
    page.subscriptions page.model
        |> Sub.map (PageMsg >> page.wrapMsg)
