import           Options.Applicative (ParserInfo, (<**>), customExecParser, fullDesc, header,
                                      helper, info, prefs, showHelpOnEmpty)

import           Cardano.RTView (runCardanoRTView)
import           Cardano.RTView.CLI (RTViewParams, parseRTViewParams)

main :: IO ()
main = do
  rtViewParams <- customExecParser (prefs showHelpOnEmpty) rtViewInfo
  runCardanoRTView rtViewParams
 where
  rtViewInfo :: ParserInfo RTViewParams
  rtViewInfo = info (parseRTViewParams <**> helper)
                    (fullDesc <> header "cardano-rt-view - real-time view for cardano node.")
