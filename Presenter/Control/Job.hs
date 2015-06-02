module Presenter.Control.Job where

import Import

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time.Clock
import Control.Monad ( forM )
import Data.Char (isAlphaNum)
import Data.Hashable
import Data.List ( sort )
import System.Random

import qualified Presenter.Registration as R
import Presenter.StarExec.Commands
import Presenter.StarExec.Connection

data Selection = SelectionCompetition | SelectionDemonstration 
    deriving (Eq, Ord, Read, Show)

data JobControl = JobControl
   { isPublic :: Bool
   , selection :: Selection
   , queue :: Int
   , space :: Int
   , wallclock :: Int
   , family_lower_bound :: Int
   , family_upper_bound :: Int
   , family_factor :: Double
   , env :: Env
   } deriving Show

num_cores :: Int
num_cores = 4

tpdb_9_0_1 :: FilePath
tpdb_9_0_1 = "TPDB-65df8a308dd6_XML.zip"

default_space :: FilePath
default_space = tpdb_9_0_1

type SpaceMap = M.Map Int Space

getSpaceMap :: FilePath -> Handler SpaceMap
getSpaceMap fp = do
  Just sp <- getDefaultSpaceXML fp
  let subspaces s = (spId s, s) : ( children s >>= subspaces )
  return $ M.fromList $ subspaces sp

timed :: Show a => a -> Competition -> Competition
timed now (Competition meta mcs) = 
    let meta' = meta { getMetaDescription 
                 = T.unwords [ getMetaDescription meta, "(", T.pack $ show now, ")"] }
    in  Competition meta' mcs

pushcat :: JobControl -> R.Category R.Catinfo -> Handler (R.Category ( R.Catinfo, [Int] ))
pushcat config cat = do
  sm <- getSpaceMap default_space
  --let ci = R.contents cat
  now <- liftIO getCurrentTime
  con <- getConnection
  jobs <- mkJobs sm config cat now
  js <- pushJobXML con (space config) jobs
  return $ cat { R.contents = (R.contents cat, catMaybes $ map jobid js) }

pushmetacat :: JobControl -> R.MetaCategory R.Catinfo -> Handler (R.MetaCategory (R.Catinfo, [Int]))
pushmetacat config mc = do
  sm <- getSpaceMap default_space
  now <- liftIO getCurrentTime
  jobs <- forM (R.categories mc) $ \ cat ->  do 
          mkJobs sm config cat now
  con <- getConnection
  js <- pushJobXML con (space config) $ concat jobs
  let m = M.fromList $ do
          SEJob { description = d, jobid = Just i } <- js
          return ( d, [i] ) 
  return $ mc {
              R.categories = for (R.categories mc) $ \ cat -> 
                cat {
                  R.contents = (R.contents cat, M.findWithDefault [] (repair $ R.categoryName cat) m )
                }
              }

pushcomp :: JobControl -> R.Competition R.Catinfo -> Handler (R.Competition (R.Catinfo, [Int]))
pushcomp config c = do
    sm <- getSpaceMap default_space
    now <- liftIO getCurrentTime
    jobs <- forM ( R.metacategories c >>= R.categories ) $ \ cat -> do 
            mkJobs sm config cat now
    con <- getConnection
    js <- pushJobXML con (space config) $ concat jobs
    let m = M.fromList $ do
            SEJob { description = d, jobid = Just i } <- js
            return ( d, [i] ) 
    return $ c {
                R.metacategories = for (R.metacategories c) $ \ mc -> 
                  mc {
                    R.categories = for (R.categories mc) $ \ cat -> 
                      cat {
                        R.contents = (R.contents cat, M.findWithDefault [] (repair $ R.categoryName cat) m )
                      }
                  }
             }

repair :: Text -> Text
repair = T.map ( \ c -> if isAlphaNum c then c else ' ' )

compact :: Text -> Text
compact = T.unwords . map (T.take 5) . T.words

for :: [a] -> (a -> b) -> [b]
for = flip map

getSpaceXMLquick :: M.Map Int Space -> Int -> Handler (Maybe Space)
getSpaceXMLquick sm sId = 
    case M.lookup sId sm of
        Just s -> return $ Just s
        Nothing -> do
            con <- getConnection
            getSpaceXML con sId

convertC :: R.Category (R.Catinfo, [Int]) -> Category
convertC c = 
  let (catInfo, jobs) = R.contents c
      name = R.categoryName c
      postProcId = R.postproc catInfo
      scoring = if 0 < (T.count "complex" $ T.toLower name)
                  then Complexity
                  else Standard
  in Category name scoring postProcId $ StarExecJobID <$> jobs

convertMC :: R.MetaCategory (R.Catinfo, [Int]) -> MetaCategory
convertMC mc = MetaCategory (R.metaCategoryName mc)
         $ map convertC (R.categories mc)

convertComp :: R.Competition (R.Catinfo,  [Int]) -> Competition
convertComp c = Competition ( CompetitionMeta (R.competitionName c ) "(missing description)" )
          $ map convertMC (R.metacategories c)

mkJobs :: SpaceMap -> JobControl -> R.Category R.Catinfo -> UTCTime -> Handler [ StarExecJob ]
mkJobs sm config cat now = do
    let ci = R.contents cat 
        (+>) = T.append
    bss <- select_benchmarks sm config $ R.benchmarks ci

    -- FIXME: too many separate jobs give problems 

    return $ return $ SEJob 
         { postproc_id = R.postproc ci
         , description = repair $ R.categoryName cat
         , job_name = compact $ repair $ R.categoryName cat +> "@" +> T.pack (show $ hash (bss, show now) )
         , queue_id = queue config
         , mem_limit = 128.0
         , wallclock_timeout = wallclock config
         , cpu_timeout = num_cores * wallclock config
         , start_paused = False
         , jobpairs = do 
               (jobspace, bs) <- bss  
               b <- sort bs
               R.Participant { R.solver_config = Just (_,c) } <- R.participants ci
               return $ SEJobPair
                          { jobPairSpace = jobspace
                          , jobPairBench = b
                          , jobPairConfig = c
                          }
         , jobid = Nothing
         }

select_benchmarks :: SpaceMap -> JobControl -> [R.Benchmark_Source] -> Handler [(Text,[Int])]
select_benchmarks sm config bs = do
    bmss <- forM bs $ \ b -> case b of
        R.Bench { R.bench = sId } -> do
            return [("root", [sId]) ]
        R.All { R.space = sId } -> do
            sp <- getSpaceXMLquick sm sId
            return $ case sp of
                Nothing -> []
                Just s -> [ (spName s, benchmarks s) ]
        R.Hierarchy { R.space = sId } -> do
            sp <- getSpaceXMLquick sm sId
            return $ case sp of
                Nothing -> []
                Just s -> families s
            
    let given = concat bmss
    result <- forM given $ select_from_family config

    liftIO $ putStrLn $ unlines
       [ "benchmark sources: " ++ show bs
       , "familiy sizes (given): " 
                  ++ show (map (\(p,bs') -> (p,length bs')) given)
       , "familiy sizes (selected): " 
                  ++ show (map (\(p,bs') -> (p,length bs')) result)
       ]

    return $ result

select_from_family :: JobControl -> (Name, [Int]) -> Handler (Name, [Int])
select_from_family config (jobspace, bms)= do
    let given = length bms
        part = round 
             $ family_factor config * fromIntegral given
        selected = 
            if part < family_lower_bound config 
            then family_lower_bound config 
            else if part > family_upper_bound config 
            then family_upper_bound config 
            else part
    bms' <- liftIO $ permute bms
    return ( jobspace, take selected  bms' )

permute :: [a] -> IO [a]
permute [] = return []
permute (x:xs) = do
    ys <- permute xs
    k <- randomRIO (0,length ys)
    let (pre,post) = splitAt k ys
    return $ pre ++ x : post
