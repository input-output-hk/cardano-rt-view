module Cardano.RTView.GUI.JS.Utils
    ( copyTextToClipboard
    ) where

copyTextToClipboard :: String
copyTextToClipboard = concat
  [ "const listener = function(ev) {"
  , "  ev.preventDefault();"
  , "  ev.clipboardData.setData('text/plain', %1);"
  , "};"
  , "document.addEventListener('copy', listener);"
  , "document.execCommand('copy');"
  , "document.removeEventListener('copy', listener);"
  ]
