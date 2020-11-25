module Cardano.RTView.NodeState.CSV
    ( mkCSVWithErrorsForHref
    ) where

import qualified Data.Text as T
import           Text.CSV (printCSV)

import           Cardano.RTView.NodeState.Types (NodeError (..))

mkCSVWithErrorsForHref :: [NodeError] -> String
mkCSVWithErrorsForHref allErrors = prepareForHref csv'
 where
  csv' = printCSV $ header : body
  header = ["Timestamp", "Severity", "Message"]
  body = map errorToCSV allErrors
  errorToCSV ne = [ show $ eTimestamp ne
                  , show $ eSeverity ne
                  , eMessage ne
                  ]

prepareForHref :: String -> String
prepareForHref =
    T.unpack
  . T.replace " " "%20"
  . T.replace "," "%2C"
  . T.replace "!" "%21"
  . T.replace "#" "%23"
  . T.replace "$" "%24"
  . T.replace "&" "%26"
  . T.replace "'" "%27"
  . T.replace "(" "%28"
  . T.replace ")" "%29"
  . T.replace "*" "%2A"
  . T.replace "+" "%2B"
  . T.replace "/" "%2F"
  . T.replace ":" "%3A"
  . T.replace ";" "%3B"
  . T.replace "=" "%3D"
  . T.replace "?" "%3F"
  . T.replace "@" "%40"
  . T.replace "[" "%5B"
  . T.replace "]" "%5D"
  . T.pack
