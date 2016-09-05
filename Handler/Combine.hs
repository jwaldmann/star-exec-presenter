{-# language FunctionalDependencies, TypeSynonymInstances #-}

module Handler.Combine ( getCombineR ) where

import Import hiding (children)
import Presenter.Model.Competition
import Presenter.Model.RouteTypes
import Database.Persist.Sql (fromSqlKey)
import Text.Lucius (luciusFile)

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Control.Monad ( forM, when )
import Data.Maybe ( catMaybes )
import Prelude (init,last)

-- | gets list of competitions. compares last one with previous ones.
-- display result in order of categories of last competition.
getCombineR compids = do
  when (null compids) $ fail "need at least one argument"
  let keys = map fromSqlKey compids
  comps <- forM compids $ \ ci -> do
    Just c <- runDB $ get ci
    return $ competitionInfoCompetition c
  let icomps = zip [0 :: Int .. ] comps
  let target = Prelude.last comps
  let m = M.unionsWith (M.unionWith (++)) $ do
        -- the competition ids might have wrong order
        (i,comp) <- icomps
        return $ M.fromListWith (M.unionWith (++)) $ do
          cat <- cats comp
          return ( key cat , M.singleton i $ children cat )
      jobids v = JobIds $ concat $ M.elems v
  let scoring k = if T.isInfixOf "omplexi" k
                  then Complexity else Standard
  let smallheader = [whamlet|
      <tr>
        <th>category
        $forall (i,comp) <- icomps
          <th>comp #{keys !! i}
        <th colspan="3">virtual best for
        |]
  defaultLayout $ do
    toWidget $(luciusFile "templates/combine.lucius")
    [whamlet|
<h1>Combine Results of Several Competitions
<p>
  <table>
    <thead>
      <tr>
        <th>index
        <th>competition
        <th>info
    <tbody>
      $forall (i,comp) <- icomps
        <tr>
          <td>#{keys !! i}
          <td>#{getMetaName $ key comp}
          <td>#{getMetaDescription $ key comp}
<p>          
  <table>
    <tbody>
      $forall mc <- children target
        <tr>
          <td colspan="#{4 + length compids}">meta-category #{key mc}
        ^{smallheader}
        $forall c <- children mc
          $maybe v <- M.lookup (key c) m
            <tr>
              <td>#{key c}
              $forall (i,comp) <- icomps
                <td>
                  $maybe j <- M.lookup i v
                    <a href=@{ShowManyJobResultsR (scoring $ key c) NoQuery $ JobIds j}>#{show j}
              <td><a href=@{ShowManyJobResultsR (scoring $ key c) (Query [VBestInit]) $ jobids v}>all but last</a>
              <td><a href=@{ShowManyJobResultsR (scoring $ key c) (Query [VBestAll]) $ jobids v}>all</a>
              <td><a href=@{ShowManyJobResultsR (scoring $ key c) NoQuery $ jobids v}>none</a>
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


