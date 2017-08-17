-- | see discussion at
-- https://github.com/stefanvonderkrone/star-exec-presenter/issues/84
-- http://lists.lri.fr/pipermail/termtools/2015-August/001070.html

module Presenter.DOI where

import Prelude
import Presenter.DOI.Type as DOI
import Presenter.Model.StarExec
import Presenter.StarExec.Space (getDefaultSpaceXML)
import Presenter.Model.RouteTypes

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO
import Control.Monad ( forM )
import Data.Monoid ((<>))

data DOIService =  DOIService
   { toDOI :: BenchmarkID -> Maybe DOI
   , toName :: DOI -> Maybe T.Text
   , toBench :: DOI -> Maybe BenchmarkID
   }

-- | reads a space built from a file like "TPDB-10.3_XML.zip"
-- returns a map with entries like
-- (StarExecBenchmarkID 2194993,"TRS_Standard/AProVE_04/JFP_Ex31.xml").
-- It will ignore the very top spName (which is TPDB-10.3 in this case)
spaceToNames :: Space -> M.Map BenchmarkID T.Text
spaceToNames sp =
  let go f sp =
        let h t = f $ spName sp <> "/" <> t
        in do (i,n) <- benchmarks_with_names sp ; return (StarExecBenchmarkID i, h n)
           ++ ( children sp >>=  go h )
  in  M.fromListWith (error "spaceToNames.duplicate") $ children sp >>= go id

-- | will read TPDB_*.zip files in order,
-- assign numbers (increasing sequence)
-- and output translation maps.
-- handle duplicates in the correct way.
-- watch out: assigned numbers depend on file contents and order.
makeDOI :: [ FilePath ] -> IO DOIService
makeDOI fs = do
  ms <- forM fs $ \ f -> do
    hPutStrLn stderr $ unwords [ "reading space file", f ]
    Just sp <- getDefaultSpaceXML f
    let out = spaceToNames sp
    hPutStrLn stderr $ unwords [ f, "contains", show (M.size out), "benchmarks" ]
    return out
  let m :: M.Map BenchmarkID T.Text
      m = foldr (M.unionWith $ error "makeDOI.duplicate") M.empty ms

      unique_names = S.toList $ S.fromList $ M.elems m

      fore :: M.Map DOI T.Text
      fore = M.fromList $ zip (map DOI.makeTPI [1..]) unique_names
      back :: M.Map T.Text DOI
      back = M.fromList $ zip unique_names (map DOI.makeTPI [1..])

      todoi = M.map ( back M.! ) m
      mm = M.fromList $ do (k,v) <- M.toList todoi ; return (v,k)

  return $ DOIService
    { toDOI = flip M.lookup todoi
    , toName = flip M.lookup fore
    , toBench = flip M.lookup mm
    }

makeDOI_for_2014_2015 :: IO DOIService
makeDOI_for_2014_2015 =
  makeDOI [ "TPDB-65df8a308dd6_XML.zip" , "TPDB-10.3_XML.zip" ]

makeDOI_for_2014_2015_extra :: IO DOIService
makeDOI_for_2014_2015_extra =
  makeDOI [ "TPDB-65df8a308dd6_XML.zip" , "TPDB-10.3_XML.zip", "johannes_waldmann_tpdb-8.0.7_XML.zip" ]

makeDOI_for_2016 :: IO DOIService
makeDOI_for_2016 =
  makeDOI [ "TPDB-65df8a308dd6_XML.zip" , "TPDB-10.3_XML.zip", "johannes_waldmann_tpdb-8.0.7_XML.zip", "TPDB-10.4_XML.zip" ]

makeDOI_for_2017 :: IO DOIService
makeDOI_for_2017 =
  makeDOI [ "TPDB-65df8a308dd6_XML.zip" , "TPDB-10.3_XML.zip", "johannes_waldmann_tpdb-8.0.7_XML.zip", "TPDB-10.4_XML.zip", "TPDB-10.5_XML.zip" ]
