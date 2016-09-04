{-# language FunctionalDependencies, TypeSynonymInstances #-}

module Handler.Combine ( getCombineR ) where

import Import hiding (children)
import Presenter.Model.Competition
import Presenter.Model.RouteTypes

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Control.Monad ( forM )
import Data.Maybe ( catMaybes )
import Prelude (last)

-- | gets list of jobs. compares last one with previous ones
getCombineR comps = do
  cs <- catMaybes <$> runDB ( forM comps get )
  defaultLayout $ combines
        $ map competitionInfoCompetition cs

combines [] = do
  fail "need at least one argument"
combines comps = do
  let m = M.unionsWith (++) $ do
        comp <- comps
        return $ M.fromListWith (++) $ do
          cat <- cats comp
          return ( key cat , children cat )
  let scoring k = if T.isInfixOf "omplexi" k
                  then Complexity else Standard
  [whamlet|
<ul>
  $forall c <- cats (last comps)
    $maybe v <- M.lookup (key c) m
      <li>#{key c} #{show v} #
        compare jobs
        : <a href=@{ShowManyJobResultsR (scoring $ key c) NoQuery $ JobIds v}>all expanded</a>
        | <a href=@{ShowManyJobResultsR (scoring $ key c) (Query [VBestInit]) $ JobIds v}>last expanded</a>
        | <a href=@{ShowManyJobResultsR (scoring $ key c) (Query [VBestAll]) $ JobIds v}>all folded</a>
|]


-- FIXME: this belongs in the module where the types are defined
class Node n k c | n -> k, n -> c where
  key :: n -> k
  children :: n -> [c]

instance Node Competition CompetitionMeta MetaCategory where
  key = getMetaData ; children = getMetaCategories
instance Node MetaCategory Name Category where
  key = getMetaCategoryName ; children = getCategories
instance Node Category Name JobID where
  key = getCategoryName ; children = getJobIds


class Cats a where cats :: a -> [ Category ]

instance Cats Category where cats c = [c]
instance Cats MetaCategory where cats m = children m >>= cats
instance Cats Competition where cats c = children c >>= cats


