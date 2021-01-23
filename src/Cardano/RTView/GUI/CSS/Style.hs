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

  h2 ? do
    fontFamily        ["Roboto Condensed"] [sansSerif]
    fontSizePx        23

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

  cl RequiredInput ? do
    color             red
    fontWeight        bold
    paddingLeftPx     5

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

  cl TopNavDropdownIcon ? do
    widthPx           12
    marginLeftPx      6
    maxHeightPx       18

  cl RTViewInfoIcon ? do
    widthPx           25
    paddingTopPx      9
    cursor            pointer

  cl NotificationsIcon ? do
    widthPx           23
    paddingTopPx      9
    marginLeftPx      4
    cursor            pointer

  cl NotificationsIconSlash ? do
    widthPx           31
    paddingTopPx      9
    cursor            pointer

  cl NotificationsEventsHeader ? do
    fontSizePct       115
    paddingBottomPx   8

  cl NotificationsHR ? do
    borderTop         solid (px 2) "#ddd"
    marginTopPx       17
    marginBottomPx    10

  cl NotificationsInput ? do
    color             "#444"
    fontSizePct       98

  cl NotificationsMainSwitch ? do
    paddingTopPx      21
    paddingBottomPx   17

  cl NotificationsSwitch ? do
    paddingTopPx      8
    paddingBottomPx   8

  cl NotificationsSwitches ? do
    paddingLeftPx     13

  cl NotificationsVSpacer ? do
    paddingTopPx      22

  cl RTViewInfoCopyPathIcon ? do
    widthPx           12
    marginLeftPx      10
    marginBottomPx    4
    cursor            pointer

  cl RTViewInfoClose ? do
    widthPx           32
    paddingRightPx    18
    paddingTopPx      16
    cursor            pointer

  cl RTViewInfoTop ? do
    backgroundColor   "#364679"

  cl RTViewInfoContainer ? do
    color             black
    fontSizePx        20

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
    cursor            help

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

  cl NotificationsBar ? do
    important $
      backgroundColor "#364679"
    important $
      color           whitesmoke
    fontSizePct       108

  cl NotificationsBar ** cl W3Button # hover ? do
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

  cl NotificationsTabContainer ? do
    paddingTopPx      16
    paddingBottomPx   16
    color             black

  cl ErrorsSortIcon ? do
    widthPx           15
    marginLeftPx      10
    maxHeightPx       19
    cursor            pointer

  cl ErrorsFilterIcon ? do
    widthPx           15
    marginLeftPx      10
    maxHeightPx       19

  cl ErrorsFilterDropdownIcon ? do
    widthPx           11
    marginLeftPx      1
    maxHeightPx       17

  cl ErrorsDownloadIcon ? do
    widthPx           15
    marginLeftPx      10
    maxHeightPx       17
    cursor            pointer

  cl ErrorsRemoveIcon ? do
    widthPx           15
    maxHeightPx       19
    cursor            pointer

  cl ErrorsBadge ? do
    color             white
    backgroundColor   red
    fontSizePct       75
    position          absolute
    marginLeftPx      (-8)
    marginTopPx       (-7)

  cl ErrorsTabHeader ? do
    marginBottomPx    15

  cl ErrorsTabList ? do
    overflowY         scroll
    heightPx          260

  cl ErrorRow ? do
    marginBottomPx    10

  cl ErrorTimestamp ? do
    fontWeight        bold

  cl WarningMessage ? do
    fontWeight        normal

  cl ErrorMessage ? do
    fontWeight        normal

  cl CriticalMessage ? do
    fontWeight        normal

  cl AlertMessage ? do
    fontWeight        normal

  cl EmergencyMessage ? do
    fontWeight        normal

  cl WarningMessageTag   ? errorTag darkorange 2 2 help
  cl ErrorMessageTag     ? errorTag darkviolet 4 5 help
  cl CriticalMessageTag  ? errorTag deeppink   4 4 help
  cl AlertMessageTag     ? errorTag orangered  4 4 help
  cl EmergencyMessageTag ? errorTag red        4 5 help

  cl WarningMessageTagNoHelp   ? errorTag darkorange 2 2 pointer
  cl ErrorMessageTagNoHelp     ? errorTag darkviolet 4 5 pointer
  cl CriticalMessageTagNoHelp  ? errorTag deeppink   4 4 pointer
  cl AlertMessageTagNoHelp     ? errorTag orangered  4 4 pointer
  cl EmergencyMessageTagNoHelp ? errorTag red        4 5 pointer

  cl NodeContainer ? do
    minHeightPx       502
    backgroundColor   "#eeeeee"

  cl NodeMenuIcon ? do
    widthPx           26

  cl ResourcesIcon ? do
    widthPx           32
    paddingRightPx    13

  cl ResourcesDropdownIcon ? do
    widthPx           15
    marginLeftPx      3
    maxHeightPx       19

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

  cl ChartArea ? do
    important $
      widthPct        100

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

  cl ProgressBar ?    progressBarColors greenDark  white
  cl ProgressBarBox ? progressBarColors greenLight white

  cl SwitchContainer ? do
    widthPx           68

  cl Switch ? do
    position          relative
    display           inlineBlock
    widthPx           52
    heightPx          25

  cl Switch ** input ? do
    widthPx           0
    heightPx          0
    opacity           0

  cl Slider ? do
    position          absolute
    cursor            pointer
    topPx             0
    leftPx            0
    rightPx           0
    bottomPx          0
    backgroundColor   "#ccc"
    transition        "" (sec 0.2) linear (sec 0)

  cl Slider # before ? do
    position          absolute
    content           (stringContent "")
    heightPx          18
    widthPx           19
    leftPx            4
    bottomPx          4
    backgroundColor   white
    transition        "" (sec 0.2) linear (sec 0)

  input # checked |+ cl Slider ? do
    backgroundColor   "#1fc1c3"

  input # checked |+ cl Slider # before ? do
    transform         (translateX $ px 25)

  cl Slider ? do
    borderRadiusPx    34

  cl Slider # before ? do
    borderRadiusPct   50

  cl TestEmailContainer ? do
    marginTopPx       20
    marginBottomPx    2

  cl TestEmailButtonArea ? do
    widthPx           120

  cl TestEmailButton ? do
    backgroundColor   "#364679"
    color             whitesmoke

  cl TestEmailDismiss ? do
    cursor            pointer
    textDecoration    underline
    color             "#364679"
    fontSizePct       90

  cl TestEmailResult ? do
    marginTopPx       8

  cl TestEmailResultSuccess ? do
    color             green

  cl TestEmailResultError ? do
    color             red

  cl AllTabsIcon ? do
    widthPx           17
    marginRightPx     13
    marginBottomPx    3

  cl SearchErrorArea ? do
    marginBottomPx    19
    marginTopPx       16
    marginLeftPx      (-4)

  cl SearchErrorIcon ? do
    widthPx           19
    position          absolute
    marginTopPx       12
    marginLeftPx      6

  cl SearchErrorInput ? do
    background        transparent
    fontSizePct       90
    paddingLeftPx     33
    maxWidthPct       50
    position          relative
    zIndex            1
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
  -- heightPct       = height . pct
  minHeightPx     = minHeight . px
  maxHeightPx     = maxHeight . px

  widthPx         = width . px
  widthPct        = width . pct
  minWidthPx      = minWidth . px
  maxWidthPx      = maxWidth . px
  maxWidthPct     = maxWidth . pct

  topPx           = top . px
  leftPx          = left . px
  rightPx         = right . px
  bottomPx        = bottom . px

  borderRadiusPx  v = borderRadius (px v) (px v) (px v) (px v)
  borderRadiusPct v = borderRadius (pct v) (pct v) (pct v) (pct v)

  progressBarColors bg c = do
    backgroundColor   bg
    important $ color c

  errorTag aColor padLeft padRight cur = do
    fontSizePct       85
    color             white
    marginRightPx     15
    paddingTopPx      2
    paddingBottomPx   0
    paddingLeftPx     padLeft
    paddingRightPx    padRight
    backgroundColor   aColor
    border            solid (px 1) aColor
    borderRadiusPx    4
    cursor            cur

-- | Convert class name as a constructor to 'Selector'.
cl :: HTMLClass -> Selector
cl className = selectorFromText $ "." <> pack (show className)
