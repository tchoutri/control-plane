{-# LANGUAGE DuplicateRecordFields #-}
module Web.View.Job where

import Data.Time
import Data.Vector (Vector)
import Text.Mustache (ToMustache (..), object, (~>))

import Job.DB (Job (..), Payload (..), Website (Website))
import Web.Templates (render)
import Web.Types
import Data.Maybe (fromJust)
import Web.Scotty.Internal.Types (ActionT)

data TemplateJob
  = TemplateJob { jobId     :: Text
                , jobName   :: Text
                , createdAt :: Text
                , runDate   :: Text
                , locked    :: Text
                }
  deriving stock (Eq, Show)

instance ToMustache TemplateJob where
  toMustache TemplateJob{..} = object [ "jobId"     ~> jobId
                                      , "jobName"   ~> jobName
                                      , "createdAt" ~> createdAt
                                      , "runDate"   ~> runDate
                                      , "locked"    ~> locked
                                      ]

indexView :: Vector Job -> ActionT LText WebM LText
indexView jobs = render "Jobs/index.mst" $ object [ "jobs" ~> js ]
  where
    js :: Vector TemplateJob
    js = fmap toTemplate jobs

newView :: ActionT LText WebM LText
newView = render "Jobs/new.mst" $ object []

toTemplate :: Job -> TemplateJob
toTemplate Job{jobId, payload, createdAt=ca, runDate=rd, lockedAt=la} =
  TemplateJob{jobId=jId, jobName = formatPayload payload, createdAt = formatDateTime ca, runDate = formatDateTime rd, locked = locked}
  where
    locked = maybe "N/A" formatDateTime la
    formatPayload :: Payload -> Text
    formatPayload (CheckWebsite (Website url)) = "CheckWebsite " <> url
    formatDateTime :: UTCTime -> Text
    formatDateTime utctime = toText $ formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" utctime
    jId = show . fromJust $ jobId
