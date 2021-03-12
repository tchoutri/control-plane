module Network.Wai.Middleware.CSP (cspHeaders) where

import qualified Data.Text as T
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import qualified Relude.Unsafe as U

cspHeaders :: Middleware
cspHeaders = addHeaders
  [ ("Content-Security-Policy", encodeUtf8 $ glue policies)
  , ("Access-Control-Allow-Origin", "*")
  ]
  where
    glue :: [Text] -> Text
    glue []  = "default-src 'none'"
    glue [x] = x
    glue xs  = T.concat $ map (\x -> T.concat [x, "; "]) (U.init xs) ++ [U.last xs]

    policies :: [T.Text]
    policies = [ "default-src 'self'"
               , "script-src 'self' 'unsafe-inline' "
                  <> " https://cdn.mathjax.org"
                  <> " https://*.twitter.com https://cdn.syndication.twimg.com"
                  <> " https://gist.github.com"
               , "img-src 'self' https: data: platform.twitter.com"
                 <> " https://dogperday.com"
               , "font-src 'self' data: https://use.typekit.net"
                 <> " https://cdn.mathjax.org"
               , "style-src 'self' 'unsafe-inline' https://use.typekit.net"
                 <> " platform.twitter.com https://assets-cdn.github.com"
               , "frame-src https://www.youtube.com https://www.slideshare.net"
               ]
