module Control.Job where

import Import

import qualified Data.Text as T

import StarExec.Registration 
import StarExec.Commands 
    (pushJobXML, getSpaceXML, getDefaultSpaceXML)
--import StarExec.Types
import StarExec.Connection (getConnection)
import Presenter.RouteTypes (JobIds(..))
import qualified StarExec.Types as S

import Data.Time.Clock
import Control.Monad ( guard, forM )
import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import System.Random
import Data.Hashable
import Control.Applicative ((<$>))
import Data.List ( sort )

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
   } 
        deriving Show

num_cores :: Int
num_cores = 4

tpdb_9_0_1 = "TPDB-65df8a308dd6_XML.zip"
default_space = tpdb_9_0_1

type SpaceMap = M.Map Int S.Space

getSpaceMap :: FilePath -> Handler SpaceMap
getSpaceMap fp = do
    Just s <- getDefaultSpaceXML fp
    let subspaces s = (S.spId s, s) : ( S.children s >>= subspaces )
    return $ M.fromList $ subspaces s

pushcat :: JobControl -> Category Catinfo -> Handler (Category ( Catinfo, [Int] ))
pushcat config cat = do
    sm <- getSpaceMap default_space
    let ci = contents cat
    now <- liftIO getCurrentTime
    con <- getConnection
    jobs <- mkJobs sm config cat now
    js <- pushJobXML con (StarExec.Types.SEJob.space config) jobs
    return $ cat { contents = (contents cat, catMaybes $ map jobid js) }

pushmetacat config mc = do
    sm <- getSpaceMap default_space
    now <- liftIO getCurrentTime
    jobss <- forM (categories mc) $ \ cat ->  do 
            mkJobs sm config cat now
    con <- getConnection
    js <- pushJobXML con (StarExec.Types.SEJob.space config) $ concat jobss
    let m = M.fromList $ do
            j @ Job { description = d, jobid = Just i } <- js
            return ( d, [i] ) 
    return $ mc { categories = for (categories mc) $ \ cat -> 
                 cat { contents = (contents cat, M.findWithDefault [] (repair $ categoryName cat) m ) } } 

pushcomp config c = do
    sm <- getSpaceMap default_space
    now <- liftIO getCurrentTime
    jobss <- forM ( metacategories c >>= categories ) $ \ cat -> do 
            mkJobs sm config cat now
    con <- getConnection
    js <- pushJobXML con (StarExec.Types.SEJob.space config) $ concat jobss
    let m = M.fromList $ do
            j @ SEJob { description = d, jobid = Just i } <- js
            return ( d, [i] ) 
        for = flip map
    return $ c { metacategories = for (metacategories c) $ \ mc -> 
             mc { categories = for (categories mc) $ \ cat -> 
                 cat { contents = (contents cat, M.findWithDefault [] (repair $ categoryName cat) m ) } } }

repair = T.map ( \ c -> if isAlphaNum c then c else ' ' )

compact = T.unwords . map (T.take 5) . T.words

for = flip map

getSpaceXMLquick sm id = 
    case M.lookup id sm of
        Just s -> return $ Just s
        Nothing -> do
            con <- getConnection
            getSpaceXML con id

select_benchmarks :: SpaceMap
                  -> JobControl -> [Benchmark_Source] 
                  -> Handler [(Text,[Int])]
select_benchmarks sm config bs = do
    bmss <- forM bs $ \ b -> case b of
        Bench { bench = id } -> do
            return [("root", [id]) ]
        All { StarExec.Registration.space = id } -> do
            s <- getSpaceXMLquick sm id
            return $ case s of
                Nothing -> []
                Just s -> [ (S.spName s, S.benchmarks s) ]
        Hierarchy { StarExec.Registration.space = id } -> do
            s <- getSpaceXMLquick sm id
            return $ case s of
                Nothing -> []
                Just s -> S.families s
            
    let given = concat bmss
    result <- forM given $ select_from_family config

    liftIO $ putStrLn $ unlines
       [ "benchmark sources: " ++ show bs
       , "familiy sizes (given): " 
                  ++ show (map (\(p,bs) -> (p,length bs)) given)
       , "familiy sizes (selected): " 
                  ++ show (map (\(p,bs) -> (p,length bs)) result)
       ]

    return $ result

-- | random subset of size given by parameters
select_from_family :: JobControl -> (S.Name, [Int]) -> Handler (S.Name, [Int])
select_from_family config (jobspace, bms) = do
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

permute [] = return []
permute (x:xs) = do
    ys <- permute xs
    k <- randomRIO (0,length ys)
    let (pre,post) = splitAt k ys
    return $ pre ++ x : post

mkJobs :: SpaceMap 
       -> JobControl -> Category Catinfo -> UTCTime 
       -> Handler [ SEJob ]
mkJobs sm config cat now = do
    let ci = contents cat 
        (+>) = T.append
    bss <- select_benchmarks sm config $ benchmarks ci

    -- FIXME: too many separate jobs give problems 

    return $ return $ SEJob 
         { postproc_id = postproc ci
         , description = repair $ categoryName cat
         , job_name = compact $ repair $ categoryName cat +> "@" +> T.pack (show $ hash (bss, show now) )
         , queue_id = queue config
         , mem_limit = 128.0
         , wallclock_timeout = wallclock config
         , cpu_timeout = num_cores * wallclock config
         , start_paused = False
         , jobpairs = do 
               (jobspace, bs) <- bss  
               b <- sort bs
               Participant { solver_config = Just (s,c) } <- participants ci
               return $ SEJobPair { jobPairSpace = jobspace, jobPairBench = b, jobPairConfig = c }
         , jobid = Nothing
         }

timed now (S.Competition meta mcs) = 
    --let name' = T.unwords [ name, "(", T.pack $ show now , ")" ]
    let meta' = meta { S.getMetaDescription 
                 = T.unwords [ S.getMetaDescription meta, "(", T.pack $ show now, ")"] }
    in  S.Competition meta' mcs

convertComp :: Competition (Catinfo,  [Int]) 
        -> S.Competition
convertComp c = S.Competition ( S.CompetitionMeta (competitionName c ) "(missing description)" )
          $ map convertMC (metacategories c)

convertMC :: MetaCategory (Catinfo, [Int])
        -> S.MetaCategory
convertMC mc = S.MetaCategory (metaCategoryName mc)
         $ map convertC (categories mc)

convertC :: Category (Catinfo, [Int])
        -> S.Category
--convertC c = S.Category (categoryName c) [ S.YES Nothing, S.NO ] 
convertC c =
  let (catInfo, jobs) = contents c
      name = categoryName c
      postProcId = postproc catInfo
      scoring = if 0 < (T.count "complex" $ T.toLower name)
                  then S.Complexity
                  else S.Standard
  in S.Category name scoring postProcId jobs

