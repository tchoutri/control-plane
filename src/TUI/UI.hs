module TUI.UI where

import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimitPercent, txt, txtWrap, withBorderStyle, (<+>))

style :: BS.BorderStyle
style = BS.unicode

sideBar :: Text -> Widget ()
sideBar content = B.border $
  hLimitPercent 15 $
    C.vCenter $ txtWrap content

mainPanel :: Text -> Widget ()
mainPanel content = B.border $
  txt content

topWidget :: Widget ()
topWidget = sideBar "The contents of the sidebar aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

ui :: Widget ()
ui = withBorderStyle style $
  topWidget
  <+> mainPanel "Main Panel"

m :: IO ()
m = M.simpleMain ui
