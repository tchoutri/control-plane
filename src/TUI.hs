module TUI where

import qualified Brick.Main as M
import TUI.UI (ui)

tuiMain :: IO ()
tuiMain = M.simpleMain ui
