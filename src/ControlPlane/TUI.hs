module ControlPlane.TUI where

import qualified Brick.Main          as M
import           ControlPlane.TUI.UI (ui)

tuiMain :: IO ()
tuiMain = M.simpleMain ui
