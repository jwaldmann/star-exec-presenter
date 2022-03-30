-- | see discussion at
-- https://github.com/stefanvonderkrone/star-exec-presenter/issues/84
-- http://lists.lri.fr/pipermail/termtools/2015-August/001070.html

module Presenter.DOI

  ( DOI(..), DOIService(), makeResolverfrom
  , fromBench, toBench, toBenches, fromName, toName
  )
  
where

import Prelude
import Presenter.DOI.Type as DOI
import Presenter.Model.StarExec
import Presenter.StarExec.Space (getDefaultSpaceXML)
import Presenter.Model.RouteTypes

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO
import Control.Monad ( forM, forM_ )
import Data.Monoid ((<>))

-- | a DOIservice can map a benchmarkID (e.g., as numbered by starexec)
-- to a canonical number (the DOI).
-- Several BenchmarkIDs can be mapped to the same DOI.
-- (this happens when identical file names are used in different uploads
-- of TPDBs).
-- The back mapping then goes to the most recent BenchmarkID
-- (in order of processing the space XML files)

data DOIService =  DOIService
   { mapToBench :: !(M.Map DOI BenchmarkID)
   , mapToBenches :: !(M.Map DOI [ BenchmarkID ] )
   , mapFromBench :: !(M.Map BenchmarkID  DOI)
   , mapToName :: !(M.Map DOI T.Text)
   , mapFromName :: !(M.Map T.Text DOI)
   }

service0 :: DOIService
service0 = DOIService
   { mapToBench = M.empty
   , mapToBenches = M.empty
   , mapFromBench = M.empty
   , mapToName = M.empty
   , mapFromName = M.empty
   }

toBench s doi = M.lookup doi (mapToBench s)
toBenches s doi = M.findWithDefault [] doi (mapToBenches s)
fromBench s bem = M.lookup bem (mapFromBench s)
toName s doi = M.lookup doi (mapToName s)
fromName s doi = M.lookup doi (mapFromName s)

-- | reads a space built from a file like "TPDB-10.3_XML.zip"
-- returns a map with entries like
-- (StarExecBenchmarkID 2194993,"TRS_Standard/AProVE_04/JFP_Ex31.xml").
-- It will ignore the very top spName (which is TPDB-10.3 in this case)
spaceToNames :: Space -> M.Map BenchmarkID T.Text
spaceToNames sp =
  M.fromListWith (error "spaceToNames.duplicate") $ spaceToNamesL sp

spaceToNamesL :: Space -> [(BenchmarkID, T.Text)]
spaceToNamesL sp = 
  let go f sp =
        let h t = f $ spName sp <> "/" <> t
        in do (i,n) <- benchmarks_with_names sp ; return (StarExecBenchmarkID i, h n)
           ++ ( children sp >>=  go h )
  in  children sp >>= go id

-- | will read TPDB_*.zip files in order,
-- assign numbers (increasing sequence)
-- and output translation maps.
-- handle duplicates in the correct way.
-- watch out: assigned numbers depend on file contents and order.
makeResolverfrom :: [ FilePath ] -> IO DOIService
makeResolverfrom fs = do
  ms <- forM fs $ \ f -> do
    hPutStrLn stderr $ unwords [ "reading space file", f ]
    Just sp <- getDefaultSpaceXML f
    let out = spaceToNamesL sp
    hPutStrLn stderr $ unwords [ f, "contains", show (length out), "benchmarks" ]
    return out
  let go s k [] = s
      go s k ((bem,can):later) =
        case M.lookup can $ mapFromName s of
          Nothing -> -- new canonical name => make new DOI
            let doi = DOI.makeTPI k
            in  go ( s { mapFromBench = M.insert bem doi $ mapFromBench s
                       , mapToBench = M.insert doi bem $ mapToBench s
                       , mapToBenches = M.insertWith (++) doi [bem] $ mapToBenches s
                       , mapFromName = M.insert can doi $ mapFromName s
                       , mapToName = M.insert doi can $ mapToName s
                       } ) (succ k) later
          Just doi -> 
            -- we already have a benchmark with that name, and it has a doi.
            -- the new benchmark will point to the known doi,
            -- and we do not change back-pointers.
            -- toBench will keep pointing to the old benchmark
            go ( s { mapFromBench = M.insert bem doi $ mapFromBench s
                   , mapToBenches = M.insertWith (++) doi [bem] $ mapToBenches s
                   } ) k later
  return $ go service0 1 $ concat ms

