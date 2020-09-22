{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.CSS.Style
    ( ownCSS
    ) where

import           Cardano.Prelude hiding (Selector, link, (**))
import qualified Prelude as P hiding ((**))

import           Clay
import           Clay.Selector (Selector, selectorFromText)
import           Data.Text (pack, unpack)
import qualified Data.Text.Lazy as TL

import           Cardano.Benchmarking.RTView.GUI.Elements (HTMLClass (..), HTMLW3Class (..))

ownCSS :: P.String
ownCSS = unpack . TL.toStrict . render $ do
  importUrl "'http://fonts.googleapis.com/css?family=Roboto Condensed'"

  body ? do
    fontFamily        ["Roboto Condensed"] [sansSerif]
    fontSizePx        20
    backgroundColor   whitesmoke

  th ? do
    important $
      paddingPx       15
    fontSizePct       110

  td ? do
    important $
      paddingPx       15

  a # link # hover ? do
    color             "#4b0082"

  a # visited # hover ? do
    color             "#4b0082"

  cl TopBar ? do
    backgroundColor   black
    color             whitesmoke
    paddingTopPx      18
    paddingBottomPx   15

  cl TopBar ** w3 W3DropdownHover ? do
    paddingTopPx      10
    fontSizePct       110

  cl TopBar ** w3 W3DropdownHover ** w3 W3Button # hover ? do
    important $
      color           iohkRed

  w3 W3DropdownHover # hover |> w3 W3Button # firstChild ? do
    important $
      color           iohkRed
    important $
      backgroundColor black

  w3 W3DropdownContent ? do
    important $
      maxHeightPx     500
    overflow          auto

  cl ServiceName ? do
    fontSizePct       120
    important $
      color           "#cccccc"
    paddingTopPx      14
    paddingRightPx    18

  cl NodeNameArea ? do
    fontSizePct       110
    paddingTopPx      10
    paddingBottomPx   10
    color             "#555555"

  cl NodeName ? do
    fontWeight        bold
    color             "#333333"

  cl NodeBar ? do
    important $
      backgroundColor "#333333"
    important $
      color           whitesmoke

  cl NodeBar ** w3 W3Button # hover ? do
    important $
      color           iohkRed

  cl ActiveTab ? do
    important $
      color           iohkRed
    fontWeight        bold

  cl TabContainer ? do
    paddingTopPx      16

  cl ErrorsTabContainer ? do
    overflowY         scroll
    heightPx          305

  cl WarningMessage ? do
    color             orange

  cl ErrorMessage ? do
    color             red

  cl CriticalMessage ? do
    color             red
    fontWeight        bold

  cl AlertMessage ? do
    color             red
    fontWeight        bold

  cl EmergencyMessage ? do
    color             red
    fontWeight        bold

  cl NodeContainer ? do
    minHeightPx       500
    backgroundColor   "#eeeeee"

  cl ReleaseName ? do
    fontWeight        bold

  cl IOHKLogo ? do
    maxWidthPx        214

  cl InfoMark ? do
    paddingLeftPx     5
    cursor            help

  cl InfoMarkImg ? do
    maxWidthPx        18

  cl DensityPercent ? do
    fontWeight        normal

  cl ValueUnit ? do
    color             gray40
    paddingLeftPx     5

  cl ValueUnitPercent ? do
    color             gray40

  cl BarValueUnit ? do
    color             whitesmoke
    paddingLeftPx     5

  cl CommitLink ? do
    color             "#4b0082"

  cl HSpacer ? do
    paddingLeftPx     10

  cl PercentsSlashHSpacer ? do
    paddingLeftPx     9
    paddingRightPx    9

  cl PercentsSlashHRSpacer ? do
    paddingRightPx    9

  cl NodeInfoVSpacer ? do
    paddingTopPx      18

  cl NodeMetricsVSpacer ? do
    paddingTopPx      15

  cl NodeInfoValues ? do
    fontWeight        bold

  cl OutdatedValue ? do
    color             gray60
    fontWeight        normal

  cl InFlight ? do
    color             "#444444"
    fontWeight        bold

  cl InFlightValues ? do
    color             "#444444"
    fontStyle         italic

  cl CPUUsageChart ? do
    maxHeightPx       190

  cl MemoryUsageChart ? do
    maxHeightPx       190

  cl DiskUsageChart ? do
    maxHeightPx       190

  cl NetworkUsageChart ? do
    maxHeightPx       190

  cl GridCPUUsageChart ? do
    maxWidthPx        280

  cl GridMemoryUsageChart ? do
    maxWidthPx        280

  cl GridDiskUsageChart ? do
    maxWidthPx        280

  cl GridNetworkUsageChart ? do
    maxWidthPx        280

  cl GridNodeNameLabel ? do
    fontWeight        normal

  cl GridRowCell ? do
    fontWeight        bold

  cl SelectNodeCheckArea ? do
    marginTopPx       8
    marginBottomPx    8
    marginLeftPx      16

  cl SelectNodeCheck ? do
    marginRightPx     10

  cl SelectMetricCheckArea ? do
    marginTopPx       8
    marginBottomPx    8
    marginLeftPx      16
    marginRightPx     16

  cl SelectMetricCheck ? do
    marginRightPx     10

  cl ProgressBar ?            progressBarColors greenDark  white
  cl ProgressBarOutdated ?    progressBarColors gray60     gray60
  cl ProgressBarBox ?         progressBarColors greenLight white
  cl ProgressBarBoxOutdated ? progressBarColors grayLight  gray60
 where
  iohkRed         = rgb 239 19  29
  gray40          = rgb 102 102 102
  gray60          = rgb 153 153 153
  grayLight       = rgb 170 170 170
  greenLight      = rgb 108 202 108
  greenDark       = rgb 0   112 0

  paddingPx v     = padding (px v) (px v) (px v) (px v)
  paddingTopPx    = paddingTop . px
  paddingBottomPx = paddingBottom . px
  paddingLeftPx   = paddingLeft . px
  paddingRightPx  = paddingRight . px

  marginTopPx     = marginTop . px
  marginBottomPx  = marginBottom . px
  marginLeftPx    = marginLeft . px
  marginRightPx   = marginRight . px

  fontSizePx      = fontSize . px
  fontSizePct     = fontSize . pct

  heightPx        = height . px
  minHeightPx     = minHeight . px
  maxHeightPx     = maxHeight . px

  maxWidthPx      = maxWidth . px

  progressBarColors bg c = do
    backgroundColor   bg
    important $ color c

-- | Convert class name as a constructor to 'Selector'.
w3 :: HTMLW3Class -> Selector
w3 className = selectorFromText $ "." <> (pack $ show className)

cl :: HTMLClass -> Selector
cl className = selectorFromText $ "." <> (pack $ show className)
