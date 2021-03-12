module Web.Templates where

import Text.Mustache (automaticCompile, checkedSubstituteValue, object, substituteValue, (~>))
import Text.Mustache.Types (Value)

import Web.Types (WebM)

templatePath :: [FilePath]
templatePath = ["./src/Web/Templates/"]

render :: FilePath -> Value -> WebM Text
render templateName vars = do
  result <- liftIO $ automaticCompile templatePath templateName
  case result of
    Left err -> error $ show err
    Right template -> do
      let (errors, renderedTemplate) = checkedSubstituteValue template vars
      if null errors
      then do
        tlResult <- liftIO $ automaticCompile templatePath "Layout/layout.mst"
        case tlResult of
          Left err -> error $ show err
          Right t  -> pure $ substituteValue t $
            object [ "template" ~> renderedTemplate ]
      else
        error $ show errors
