module Debugger.Reflect where

import Debugger.RuntimeApi as API
import Html

import Native.Debugger.Reflect

getHtml : API.JsElmValue -> Html.Html
getHtml =
  Native.Debugger.Reflect.getHtml

-- TODO: repr of Elm values in Elm