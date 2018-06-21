module Thunderfront.Forms.Login where

import Prelude

import Bonsai.Forms (FormMsg, form, passwordInput, textInput, withLegend, withMessage, (!))
import Bonsai.Forms.PureCss (alignedForm)
import Data.Lens (view)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Thunderfront.Types (class HasLoginFormModel, loginFormModel)

loginForm :: forall m. HasLoginFormModel m => m -> H.Markup FormMsg
loginForm m =
  alignedForm Nothing (view loginFormModel m) $
    form "login" `withLegend` "Login" $ do
      textInput "username" "Username" `withMessage` "Required" ! A.required true
      passwordInput "password" "Password" `withMessage` "Required" ! A.required true
