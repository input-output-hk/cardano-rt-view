module Cardano.RTView.SupportedNodes
    ( supportedNodesVersions
    , showSupportedNodesVersions
    ) where

import           Data.Text (Text, intercalate)

-- | Both RTView and 'cardano-node' are under active development.
--   Since the things change frequently, RTView maintain the list
--   of nodes' versions that works with this release of RTView.
supportedNodesVersions :: [Text]
supportedNodesVersions =
  [ "1.22.1"
  ]

showSupportedNodesVersions :: Text
showSupportedNodesVersions = intercalate ", " supportedNodesVersions
