module ControlPlane.Web.View.Root where

import Lucid

import ControlPlane.Web.View.Base

view :: Html ()
view = template $ do
  h1_ "Welcome to the control plane"
  forM_ [("Overview", "/overview"), ("Root", "/")] linkList

linkList :: (Html (),Text) -> Html ()
linkList (title, url) = li_ $ a_ [href_ url] title
