module Utils.WidgetMetaRefresh where

import Import
import Text.Blaze

-- insertWidgetMetaRefresh ::  (ToWidgetHead (HandlerSite m) (t1 -> Markup), ToWidget (HandlerSite m) (t -> Markup), MonadWidget m) =>  m ()
insertWidgetMetaRefresh = do
  toWidget [hamlet|
    <section>The Data is currently queried from star-exec. This site will constantly refresh until the data is fully loaded
  |]
  toWidgetHead [hamlet|
    <meta http-equiv="refresh" content="10">
  |]
