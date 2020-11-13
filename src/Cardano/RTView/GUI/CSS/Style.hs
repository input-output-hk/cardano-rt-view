module Cardano.RTView.GUI.CSS.Style
    ( ownCSS
    ) where

import           Prelude hiding ((**))

import           Clay
import qualified Clay.Media as M
import           Clay.Selector (selectorFromText)
import           Data.Text (pack, unpack)
import qualified Data.Text.Lazy as TL

import           Cardano.RTView.GUI.Elements (HTMLClass (..)) 

ownCSS :: String
ownCSS = unpack . TL.toStrict . render $ do
  importUrl "'http://fonts.googleapis.com/css?family=Roboto Condensed'"

  body ? do
    fontFamily        ["Roboto Condensed"] [sansSerif]
    fontSizePx        20
    backgroundColor   whitesmoke
    color "#1b2238"

  -- To avoid shifting "labels-values" on mobile screens (for Pane mode).
  query M.screen [M.maxWidth (px 601)] $ ".w3-col.m6, .w3-half" ?     widthPct 49.99
  query M.screen [M.maxWidth (px 601)] $ ".w3-col.m4, .w3-third" ?    widthPct 33.33
  query M.screen [M.maxWidth (px 601)] $ ".w3-col.m8, .w3-twothird" ? widthPct 66.66

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
    backgroundColor   "#1b2238"
    color             whitesmoke
    paddingTopPx      18
    paddingBottomPx   15

  cl TopBar ** cl W3DropdownHover ? do
    paddingTopPx      10
    fontSizePct       110

  cl TopBar ** cl W3DropdownHover ** cl W3Button # hover ? do
    important $
      color           cardanoLight

  cl W3DropdownHover # hover |> cl W3Button # firstChild ? do
    important $
      color           cardanoLight
    important $
      backgroundColor "#1b2238"

  cl W3BarItem # hover ? do
    important $
      color           cardanoLight
    important $
      backgroundColor "#1b2238"

  cl W3DropdownContent ? do
    important $
      maxHeightPx     500
    overflow          auto

  cl ServiceName ? do
    fontSizePct       120
    important $
      color           "#bcbcbc"
    paddingTopPx      12
    paddingRightPx    18

  cl IdleNode ? do
    fontWeight        bold
    color             white
    marginLeftPx      15
    paddingTopPx      3
    paddingBottomPx   1
    paddingLeftPx     5
    paddingRightPx    5
    backgroundColor   red
    border            solid (px 1) red
    borderRadiusPx    8

  cl UnsupportedVersion ? do
    color             red
    fontWeight        bold
    textDecorationLine  underline
    textDecorationStyle wavy
    textDecorationColor red

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
      backgroundColor "#364679"
    important $
      color           whitesmoke

  cl NodeBar ** cl W3Button # hover ? do
    important $
      color           cardanoLight

  cl ActiveTab ? do
    important $
      backgroundColor "#1b2238"
    important $
      color           cardanoLight
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

  cl NodeMenuIcon ? do
    widthPx           26

  cl ResourcesIcon ? do
    widthPx           32
    paddingRightPx    13

  cl ShowHideIcon ? do
    widthPx           32
    paddingRightPx    13

  cl CardanoLogo ? do
    widthPx           216

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

  cl TXsProcessed ? do
    fontWeight        bold
    paddingLeftPx     16

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

  cl NodeInfoVSpacer ? do
    paddingTopPx      18

  cl NodeMetricsVSpacer ? do
    paddingTopPx      15

  cl NodeInfoValues ? do
    fontWeight        bold

  cl NodeMetricsValues ? do
    fontWeight        bold

  cl CPUUsageChart ? do
    maxHeightPx       360

  cl MemoryUsageChart ? do
    maxHeightPx       360

  cl DiskUsageChart ? do
    maxHeightPx       360

  cl NetworkUsageChart ? do
    maxHeightPx       360

  cl GridCPUUsageChart ? do
    maxWidthPx        320

  cl GridMemoryUsageChart ? do
    maxWidthPx        320

  cl GridDiskUsageChart ? do
    maxWidthPx        320

  cl GridNetworkUsageChart ? do
    maxWidthPx        320

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

  cl MetricsArea ? do
    minWidthPx        300

  cl ProgressBar ?            progressBarColors greenDark  white
  cl ProgressBarBox ?         progressBarColors greenLight white
 where
  cardanoLight    = rgb 31  193 195
  gray40          = rgb 102 102 102
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

  widthPx         = width . px
  widthPct        = width . pct
  minWidthPx      = minWidth . px
  maxWidthPx      = maxWidth . px

  borderRadiusPx v = borderRadius (px v) (px v) (px v) (px v)

  progressBarColors bg c = do
    backgroundColor   bg
    important $ color c

-- | Convert class name as a constructor to 'Selector'.
cl :: HTMLClass -> Selector
cl className = selectorFromText $ "." <> pack (show className)
