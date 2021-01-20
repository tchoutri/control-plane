module ControlPlane.Web.View.Root where

import Lucid

import ControlPlane.DB.Job
import ControlPlane.Web.View.Base

view :: [Job] -> Html ()
view jobs = template $ do
  h1_ "Welcome to the control plane"
  jobTable jobs

jobTable :: [Job] -> Html ()
jobTable jobs =
  div_ [class_ "-my-2 py-2 overflow-x-auto sm:-mx-6 sm:px-6 lg:-mx-8 pr-10 lg:px-8"] $ do
    div_ [class_ "align-middle rounded-tl-lg rounded-tr-lg inline-block w-full py-4 overflow-hidden bg-white shadow-lg px-12"] $ do
      div_ [class_ "flex justify-between"] $
        div_ [class_ "inline-flex border rounded w-7/12 px-2 lg:px-6 h-12 bg-transparent"] $
          div_ [class_ "flex flex-wrap items-stretch w-full h-full mb-6 relative"] $ do
            div_ [class_ "flex"] $
              span_ [class_ "flex items-center leading-normal bg-transparent rounded rounded-r-none border border-r-0 border-none lg:px-3 py-2 whitespace-no-wrap text-grey-dark text-sm"] $
                svg_ [width_ "18", height_ "18", class_ "w-4 lg:w-auto", viewBox_ "0 0 18 18", fill_ "none", xmlns_ "http://www.w3.org/2000/svg"] $ do
                  path_ [d_ "M8.11086 15.2217C12.0381 15.2217 15.2217 12.0381 15.2217 8.11086C15.2217 4.18364 12.0381 1 8.11086 1C4.18364 1 1 4.18364 1 8.11086C1 12.0381 4.18364 15.2217 8.11086 15.2217Z", stroke_ "#455A64", strokeLinecap_ "round", strokeLinejoin_ "round"]
                  path_ [d_ "M16.9993 16.9993L13.1328 13.1328", stroke_ "#455A64", strokeLinecap_ "round", strokeLinejoin_ "round"]
    div_ [class_ "align-middle inline-block min-w-full shadow overflow-hidden bg-white shadow-dashboard px-8 pt-3 rounded-bl-lg rounded-br-lg"] $
      table_ [class_ "min-w-full"] $ do
        thead_ [] $ tr_ [] $ do
          th_ [class_ "px-6 py-3 border-b-2 border-gray-300 text-left text-sm leading-4 text-blue-500 tracking-wider"] "Type"
          th_ [class_ "px-6 py-3 border-b-2 border-gray-300 text-left text-sm leading-4 text-blue-500 tracking-wider"] "Created at"
          th_ [class_ "px-6 py-3 border-b-2 border-gray-300 text-left text-sm leading-4 text-blue-500 tracking-wider"] "Run date"
          th_ [class_ "px-6 py-3 border-b-2 border-gray-300 text-left text-sm leading-4 text-blue-500 tracking-wider"] "Attempts"
        tbody_ [class_ "bg-white"] $
          mapM_ jobElement jobs

jobElement :: Job -> Html ()
jobElement Job{..} =
  tr_ [] $ do
    td_ [class_ "px-6 py-4 whitespace-no-wrap border-b border-gray-500"] $
      div_ [class_ "text-sm leading-5 text-blue-900"] (show payload)
    td_ [class_ "px-6 py-4 whitespace-no-wrap border-b border-gray-500"] $
      div_ [class_ "text-sm leading-5 text-blue-900"] (show createdAt)
    td_ [class_ "px-6 py-4 whitespace-no-wrap border-b border-gray-500"] $
      div_ [class_ "text-sm leading-5 text-blue-900"] (show runDate)
    td_ [class_ "px-6 py-4 whitespace-no-wrap border-b border-gray-500"] $
      div_ [class_ "text-sm leading-5 text-blue-900"] (show attempts)
