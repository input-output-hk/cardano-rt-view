{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
import           System.IO (hSetEncoding, stdout, stderr, utf8)
import           System.Win32.Console (setConsoleCP)
#endif

import           Data.Version (showVersion)
import           Options.Applicative (ParserInfo, (<**>), customExecParser, fullDesc, header,
                                      help, helper, info, infoOption, long, prefs, short,
                                      showHelpOnEmpty)

import           Cardano.RTView (runCardanoRTView)
import           Cardano.RTView.CLI (RTViewParams, parseRTViewParams)
import           Paths_cardano_rt_view (version)

main :: IO ()
main = do
#if defined(mingw32_HOST_OS)
  -- Unfortunately, the terminal in Windows 10 isn't UTF8-ready by default.
  -- Set encoding and code page explicitly.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  setConsoleCP 65001
#endif
  rtViewParams <- customExecParser (prefs showHelpOnEmpty) rtViewInfo
  runCardanoRTView rtViewParams
 where
  rtViewInfo :: ParserInfo RTViewParams
  rtViewInfo = info
    (parseRTViewParams <**> helper <**> versionOption)
    (fullDesc <> header "cardano-rt-view - real-time view for cardano node.")
  versionOption = infoOption
    (showVersion version)
    (long "version" <>
     short 'v' <>
     help "Show version")
