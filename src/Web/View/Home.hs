module Web.View.Home where

import Data.Time
import Data.Vector (Vector)
import Text.Mustache (ToMustache (..), object, (~>))

import Job.DB (Job (..), Payload (..), Website (Website))
import Web.Templates (render)
import Web.Types

data TemplateJob
  = TemplateJob { jobName   :: Text
                , createdAt :: Text
                , runDate   :: Text
                , locked    :: Text
                }
  deriving stock (Eq, Show)

instance ToMustache TemplateJob where
  toMustache TemplateJob{..} = object [ "jobName" ~> jobName
                                      , "createdAt" ~> createdAt
                                      , "runDate" ~> runDate
                                      , "locked" ~> locked
                                      ]

view :: Vector Job -> WebM Text
view jobs = render "Home/show.mst" $ object [ "jobs" ~> js ]
  where
    js :: Vector TemplateJob
    js = fmap toTemplate jobs

toTemplate :: Job -> TemplateJob
toTemplate Job{payload, createdAt=ca, runDate=rd, lockedAt=la} =
  TemplateJob{jobName = formatPayload payload, createdAt = formatDateTime ca, runDate = formatDateTime rd, locked = locked}
  where
    locked = maybe "N/A" formatDateTime la
    formatPayload :: Payload -> Text
    formatPayload (CheckWebsite (Website url)) = "CheckWebsite " <> url
    formatPayload (GrabJSON txt)               = "GrabJSON " <> txt
    formatDateTime :: UTCTime -> Text
    formatDateTime utctime = toText $ formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" utctime
