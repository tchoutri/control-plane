module Web.View.Base where

import Lucid
import Lucid.Base

template :: Html () -> Html ()
template arg =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "Control Plane"
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [href_ "/static/style.css", rel_ "stylesheet", type_ "text/css"]
    body_ $ do
      sideBar
      div_ [class_ ""]
        arg
      script_ [src_ "/static/script.js"] ("" :: Text)

sideBar :: Html ()
sideBar = div_ [class_ "flex flex-wrap bg-gray-100 w-full h-screen"] $
  div_ [class_ "w-3/12 bg-white rounded p-3 shadow-lg"] $ do
    div_ [class_ "flex items-center space-x-4 p-2 mb-5"] $ do
      img_ [class_ "h-12 rounded-full", src_ "/static/profile_picture.jpg", crossorigin_ "anonymous"]
      div_ [] $
        h4_ [class_ "font-semibold text-lg text-gray-700 capitalize font-poppins tracking-wide"] "Admin"
    ul_ [class_ "space-y-2 text-sm"] $ do
      overviewElement


overviewElement :: Html ()
overviewElement =
  li_ $
    a_ [href_ "#", class_ "flex items-center space-x-3 text-gray-700 p-2 rounded-md font-medium hover:bg-gray-200 bg-gray-200 focus:shadow-outline"] $ do
      span_ [class_ "text-gray-600"] $
        svg_ [class_ "h-5", xmlns_ "http://www.w3.org/2000/svg", fill_ "none", viewBox_ "0 0 24 24", stroke_ "currentColor"] $
          path_ [ strokeLinecap_ "round", strokeLinejoin_ "round" , strokeWidth_ "2"
                , d_ "M9.75 17L9 20l-1 1h8l-1-1-.75-3M3 13h18M5 17h14a2 2 0 002-2V5a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"]
      span_ [] $ toHtml ("Overview" :: Text)

strokeLinecap_ :: Text -> Attribute
strokeLinecap_ = makeAttribute "stroke-linecap"

strokeLinejoin_ :: Text -> Attribute
strokeLinejoin_ = makeAttribute "stroke-linejoin"

strokeWidth_ :: Text -> Attribute
strokeWidth_ = makeAttribute "stroke-width"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

fill_ :: Text -> Attribute
fill_ = makeAttribute "fill"

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

stroke_ :: Text -> Attribute
stroke_ = makeAttribute "stroke"

path_ :: Monad m => [Attribute] -> HtmlT m ()
path_ = with (makeElementNoEnd "path")
