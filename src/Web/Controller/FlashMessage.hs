module Web.Controller.FlashMessage where

import Web.Scotty.Trans (ActionT)
import Web.Session (getAssign, insertAssign, modifySession, readSession, removeAssign)
import Web.Types (WebEnvironment (sessions), WebM)

putInfo :: Text -> ActionT LText WebM ()
putInfo msg = putFlash "alert_info" msg

putWarning :: Text -> ActionT LText WebM ()
putWarning msg = putFlash "alert_warning" msg

putError :: Text -> ActionT LText WebM ()
putError msg = putFlash "alert_error" msg

getInfo :: ActionT LText WebM (Maybe Text)
getInfo = popFlash "alert_info"

getWarning :: ActionT LText WebM (Maybe Text)
getWarning = popFlash "alert_warning"

getError :: ActionT LText WebM (Maybe Text)
getError = popFlash "alert_error"

putFlash :: Text -> Text -> ActionT LText WebM ()
putFlash key value = do
  sm <- asks sessions
  modifySession sm (\mVal -> mVal >>= Just . insertAssign key value)

popFlash :: Text -> ActionT LText WebM (Maybe Text)
popFlash flash = do
  sm <- asks sessions
  mUserAssigns <- readSession sm
  case mUserAssigns of
    Nothing -> pure Nothing
    Just ua -> do
      let content = getAssign flash ua
      modifySession sm (\mVal -> mVal >>= Just . removeAssign flash)
      pure content
