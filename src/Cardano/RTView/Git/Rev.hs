{-# LANGUAGE TemplateHaskell #-}

module Cardano.RTView.Git.Rev
    ( gitRev
    ) where

import           Data.FileEmbed (dummySpaceWith)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

import           Cardano.RTView.Git.RevTH (gitRevFromGit)

gitRev :: String
gitRev | gitRevEmbed /= zeroRev = T.unpack gitRevEmbed
       | T.null fromGit         = T.unpack zeroRev
       | otherwise              = T.unpack fromGit
 where
  -- Git revision embedded after compilation using
  -- Data.FileEmbed.injectWith. If nothing has been injected,
  -- this will be filled with 0 characters.
  gitRevEmbed :: Text
  gitRevEmbed = decodeUtf8 $(dummySpaceWith "gitrev" 40)

  -- Git revision found during compilation by running git. If
  -- git could not be run, then this will be empty.
  fromGit = T.strip (T.pack $(gitRevFromGit))

  zeroRev :: Text
  zeroRev = "0000000000000000000000000000000000000000"
