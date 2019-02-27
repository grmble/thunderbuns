module Thunderfront.View where

import Prelude

import Bonsai (Cmd)
import Bonsai.Html (Markup, VNode, a, div_, input, li, mapMarkup, nav, render, span, text, ul, (!))
import Bonsai.Html.Attributes (cls, id_, href, typ, value)
import Bonsai.Html.Events (onClickPreventDefault, onInput, onKeyEnter)
import Bonsai.EventHandlers (enterEscapeKey)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Foreign (Foreign, F)
import Thunderfront.Forms.Login (loginForm)
import Thunderfront.Types.Model (Model, CurrentView(..), activeChannel, channels, channelName, currentView, inputModel, messages)
import Thunderfront.Types.Msg (Msg(..))
import Thunderfront.Types.WS as WS

viewMain :: Model -> VNode Msg
viewMain model = do
  render $
    div_ ! id_ "bonsai-main" ! cls "pure-g" $ do
      viewMenu model
      viewCurrentView model

viewLoginForm :: Model -> Markup Msg
viewLoginForm model =
  div_ ! cls "pure-u-1 l-box"
    -- #! style "margin-left" "2em"
  $ mapMarkup LoginFormMsg $ loginForm model

viewMenu :: Model -> Markup Msg
viewMenu model = do
  nav ! id_ "menu" ! cls "l-box pure-u-1 pure-menu pure-menu-horizontal pure-menu-scrollable" $ do
    span ! cls "pure-menu-heading" $ text "Thunderbuns"
    ul ! cls "pure-menu-list" $ do
      item ChannelView "Channels"
      item DebugView "Debug"
      -- item' "pure-menu-item" (JwtTokenMsg Nothing) "Logout"
  where
    item current str =
      item' (menuItemClasses current) (CurrentViewMsg current) str
    item' klass msg str =
      li ! cls klass $
        a ! cls "pure-menu-link" ! href "#"
          ! onClickPreventDefault msg
          $ text str
    menuItemClasses current =
      if current == (view currentView model)
          then "pure-menu-item pure-menu-selected"
          else "pure-menu-item"

viewCurrentView :: Model -> Markup Msg
viewCurrentView model =
  case (view currentView model) of
    ChannelView -> do
      viewChannels model
      viewActiveChannel model
    DebugView -> viewDebug model

viewChannels :: Model -> Markup Msg
viewChannels model = do
  div_ ! id_ "channels" ! cls "l-box pure-u-1-3 pure-u-md-1-6 pure-menu" $ do
    span ! cls "pure-menu-heading" $ text "Channels"
    ul ! cls "l-plainlist" $
    for_ (view channels model) $ \c -> do
      li ! cls (menuItemClasses c) $
        a ! cls "pure-menu-link" ! href "#"
          -- ! XXX: NO ACTION onClickPreventDefault (ActiveChannelMsg c)
          $ text (view channelName c)
  where
    menuItemClasses current =
      if (Just current) == (view activeChannel model)
          then "pure-menu-item pure-menu-selected"
          else "pure-menu-item"

viewActiveChannel :: Model -> Markup Msg
viewActiveChannel model =
  div_ ! id_ "content" ! cls "l-box pure-u-2-3 pure-u-md-5-6" $ do
    ul ! id_ "messages"
      ! cls "l-plainlist l-stretch" $
      for_ (view messages model) $ li <<< text
    div_ ! cls "pure-form" $
      input ! id_ "msgInput"
        ! cls "pure-u-1 pure-input"
        ! typ "text"
        ! value (view inputModel model)
        ! onInput MessageInputMsg
        ! onKeyEnter (\s -> RequestMsg $ (WS.GenericCommand { cmd: s }))

-- | Event handler for enter key. XXX PROMOTE
rawEnterKeyHandler :: forall msg. (String -> F (Cmd msg)) -> Foreign -> F (Cmd msg)
rawEnterKeyHandler fn ev =
  enterEscapeKey ev >>= case _ of
    Nothing -> pure empty
    Just (Left _) -> pure empty
    Just (Right s) -> fn s

viewDebug :: Model -> Markup Msg
viewDebug model = do
  div_ ! cls "l-box pure-u-1" $ do
    div_ $ text "DEBUG PANE"