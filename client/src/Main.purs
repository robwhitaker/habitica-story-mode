module Main (main) where

import Oak (Html(..), Maybe(..), View, button, div, onClick, p, text)
import Oak as Oak

import Prelude hiding (div)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Ref as Ref
import Data.Array (reverse)
import Control.Monad.Except (runExcept)
import Data.Traversable (for_, traverse_)

import Effect.Timer (setInterval)

import Foreign (readString)

import Web.Socket.WebSocket (create, sendString, toEventTarget) as WS
import Web.Socket.Event.MessageEvent (data_, fromEvent) as WS
import Web.Socket.Event.EventTypes (onMessage) as WS

import Web.Event.EventTarget as Event

type Model
    = {number :: Int, messages :: Array String}

type MsgSender
    = Msg -> Effect Unit

type Subscription
    = MsgSender -> Aff Unit

data Msg
    = Inc
    | Dec
    | Init (Ref.Ref MsgSender)
    | WebhookMessageReceived String

-- This is a fix for a bug in the Oak library where all HTML appears
-- in reverse order
fixView :: forall msg. View msg -> View msg
fixView builder =
    Oak.putBuilder (map fixHtml $ reverse $ Oak.runBuilder builder)
  where
    fixHtml :: Html msg -> Html msg
    fixHtml html =
        case html of
            Text _ ->
                html

            Tag str attrs children ->
                Tag str attrs (map fixHtml $ reverse children)


view :: Model -> View Msg
view model = fixView do
    div [] do
        button [onClick Inc] (text "+")
        div [] (text model.number)
        button [onClick Dec] (text "-")
    div [] do
       for_ model.messages (p [] <<< text)


next :: Msg -> Model -> MsgSender -> Effect Unit
next msg model sendToApp =
    case msg of
        Init ref ->
            Ref.write sendToApp ref

        _ ->
            mempty

update :: Msg -> Model -> Model
update msg model = case msg of
    Inc -> model { number = model.number + 1 }
    Dec -> model { number = model.number - 1 }
    Init _ -> model
    WebhookMessageReceived str ->
        model { messages = model.messages <> [str] }

init :: Model
init = { number: 0, messages: ["one", "two", "three"] }

webhookEvents :: Subscription
webhookEvents sendToApp = liftEffect do
    socket <- WS.create "wss://echo.websocket.org" []
    messageListener <- Event.eventListener $
        WS.fromEvent >>> traverse_
            ( WS.data_ >>> readString >>> runExcept
                >>> traverse_ (WebhookMessageReceived >>> sendToApp) )
    Event.addEventListener WS.onMessage messageListener false (WS.toEventTarget socket)
    void $ setInterval 3000 $ WS.sendString socket "hi ho hello there"

subscriptions :: Array Subscription
subscriptions =
    [ webhookEvents
    ]

main :: Effect Unit
main = do
    sendFnRef <- Ref.new \_ -> mempty
    rootNode <-
        Oak.runApp
            (Oak.createApp { init, view, update, next })
            (Just (Init sendFnRef))

    container <- Oak.getElementById "app"
    Oak.appendChildNode container rootNode

    sendFn <- Ref.read sendFnRef
    traverse_ (launchAff_ <<< (_ $ sendFn)) subscriptions
