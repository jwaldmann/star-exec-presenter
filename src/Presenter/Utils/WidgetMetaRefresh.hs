module Presenter.Utils.WidgetMetaRefresh where

import Import

-- insertWidgetMetaRefresh ::  (ToWidgetHead (HandlerSite m) (t1 -> Markup), ToWidget (HandlerSite m) (t -> Markup), MonadWidget m) =>  m ()
insertWidgetMetaRefresh :: MonadWidget m => m ()
insertWidgetMetaRefresh = do
  toWidget [hamlet|
    <section>Data shown here is incomplete. This page will refresh with updates pulled from star-exec.
  |]
  toWidgetHead [hamlet|
    <meta http-equiv="refresh" content="10">
  |]
