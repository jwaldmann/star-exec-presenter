module Control.Job where

import Import

import qualified Data.Text as T

import StarExec.Registration 
import StarExec.Commands (pushJobXML, Job (..), getSpaceXML)
import StarExec.Connection (getConnection)
import StarExec.Types (JobIds(..))
import qualified StarExec.Types as S

import Data.Time.Clock
import Control.Monad ( guard, forM )
import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import System.Random
import Data.Hashable
import Control.Applicative ((<$>))

data JobControl = JobControl
   { user :: Text
   , pass :: Text
   , isPublic :: Bool
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

pushcat :: JobControl -> Category Catinfo -> Handler (Category ( Catinfo, [Int] ))
pushcat config cat = do
    let ci = contents cat
    now <- liftIO getCurrentTime
    con <- getConnection
    jobs <- mkJobs config cat now
    js <- pushJobXML con (Control.Job.space config) jobs
    return $ cat { contents = (contents cat, catMaybes $ map jobid js) }

pushmetacat config mc = do
    now <- liftIO getCurrentTime
    jobss <- forM (categories mc) $ \ cat ->  do 
            mkJobs config cat now
    con <- getConnection
    js <- pushJobXML con (Control.Job.space config) $ concat jobss
    let m = M.fromList $ do
            j @ Job { description = d, jobid = Just i } <- js
            return ( d, [i] ) 
    return $ mc { categories = for (categories mc) $ \ cat -> 
                 cat { contents = (contents cat, M.findWithDefault [] (repair $ categoryName cat) m ) } } 

pushcomp config c = do
    now <- liftIO getCurrentTime
    jobss <- forM ( metacategories c >>= categories ) $ \ cat -> do 
            mkJobs config cat now
    con <- getConnection
    js <- pushJobXML con (Control.Job.space config) $ concat jobss
    let m = M.fromList $ do
            j @ Job { description = d, jobid = Just i } <- js
            return ( d, [i] ) 
        for = flip map
    return $ c { metacategories = for (metacategories c) $ \ mc -> 
             mc { categories = for (categories mc) $ \ cat -> 
                 cat { contents = (contents cat, M.findWithDefault [] (repair $ categoryName cat) m ) } } }

repair = T.map ( \ c -> if isAlphaNum c then c else ' ' )

compact = T.unwords . map (T.take 5) . T.words

for = flip map

-- FIXME: getspaceXML should be DB-cached (issue #29)
select_benchmarks :: JobControl -> [Benchmark_Source] 
                  -> Handler [[Int]]
select_benchmarks config bs = do
    con <- getConnection
    bmss <- forM bs $ \ b -> case b of
        Bench { bench = id } -> do
            return [[id]]
        All { StarExec.Registration.space = id } -> do
            s <- getSpaceXML con id
            return $ case s of
                Nothing -> []
                Just s -> [S.benchmarks s ]
        Hierarchy { StarExec.Registration.space = id } -> do
            s <- getSpaceXML con id
            return $ case s of
                Nothing -> []
                Just s -> S.families s
            
    let given = concat bmss
    result <- forM given $ select_from_family config

    liftIO $ putStrLn $ unlines
       [ "benchmark sources: " ++ show bs
       , "familiy sizes (given): " ++ show (map length given)
       , "familiy sizes (selected): " ++ show (map length result)
       ]

    return $ result

-- | random subset of size given by parameters
select_from_family :: JobControl -> [Int] -> Handler [Int]
select_from_family config bms = do
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
    return $ take selected  bms'

permute [] = return []
permute (x:xs) = do
    ys <- permute xs
    k <- randomRIO (0,length ys)
    let (pre,post) = splitAt k ys
    return $ pre ++ x : post

mkJobs :: JobControl -> Category Catinfo -> UTCTime 
       -> Handler [ Job ]
mkJobs config cat now = do
    let ci = contents cat 
        (+>) = T.append
    bss <- select_benchmarks config $ benchmarks ci

    -- FIXME: too many separate jobs give problems 
    let all_in_one bss = [ concat bss ]

    return $ for (all_in_one bss) $ \ bs -> Job 
         { postproc_id = postproc ci
         , description = repair $ categoryName cat
         , job_name = compact $ repair $ categoryName cat +> "@" +> T.pack (show $ hash bs )
         , queue_id = queue config
         , mem_limit = 128.0
         , wallclock_timeout = wallclock config
         , cpu_timeout = num_cores * wallclock config
         , start_paused = False
         , jobpairs = do 
               b <- bs
               Participant { solver_config = Just (s,c) } <- participants ci
               return ( b, c )
         , jobid = Nothing
         }

timed now (S.Competition name mcs) = 
    let name' = T.unwords [ name, "(", T.pack $ show now , ")" ]
    in  S.Competition name' mcs

convertComp :: Competition (Catinfo,  [Int]) 
        -> S.Competition
convertComp c = S.Competition (competitionName c) 
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
  in S.Category name S.Standard postProcId jobs

