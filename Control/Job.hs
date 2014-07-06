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

data JobControl = JobControl
   { user :: Text
   , pass :: Text
   , queue :: Int
   , space :: Int
   , wallclock :: Int
   , benchmarks_per_category :: Int
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
    job <- mkJob config cat now
    js <- pushJobXML con (Control.Job.space config) [ job ]
    return $ cat { contents = (contents cat, catMaybes $ map jobid js) }

pushmetacat config mc = do
    now <- liftIO getCurrentTime
    jobs <- forM (categories mc) $ \ cat ->  do 
            mkJob config cat now
    con <- getConnection
    js <- pushJobXML con (Control.Job.space config) jobs
    let m = M.fromList $ do
            j @ Job { description = d, jobid = Just i } <- js
            return ( d, [i] ) 
        for = flip map
    return $ mc { categories = for (categories mc) $ \ cat -> 
                 cat { contents = (contents cat, M.findWithDefault [] (repair $ categoryName cat) m ) } } 

pushcomp config c = do
    now <- liftIO getCurrentTime
    jobs <- forM ( metacategories c >>= categories ) $ \ cat -> do 
            mkJob config cat now
    con <- getConnection
    js <- pushJobXML con (Control.Job.space config) jobs
    let m = M.fromList $ do
            j @ Job { description = d, jobid = Just i } <- js
            return ( d, [i] ) 
        for = flip map
    return $ c { metacategories = for (metacategories c) $ \ mc -> 
             mc { categories = for (categories mc) $ \ cat -> 
                 cat { contents = (contents cat, M.findWithDefault [] (repair $ categoryName cat) m ) } } }

repair = T.map ( \ c -> if isAlphaNum c then c else ' ' )

compact = T.unwords . map (T.take 5) . T.words


-- FIXME: getspaceXML should be DB-cached
select_benchmarks num bs = do
    con <- getConnection
    bmss <- forM bs $ \ b -> case b of
        Bench { bench = id } -> do
            return [id]
        All { StarExec.Registration.space = id } -> do
            s <- getSpaceXML con id
            return $ case s of
                Nothing -> []
                Just s -> S.benchmarks s                
        Hierarchy { StarExec.Registration.space = id } -> do
            error "select benchmarks from hierarchy not implemented"
    let bms = concat bmss
    bms' <- liftIO $ permute bms
    return $ take num bms'

permute [] = return []
permute (x:xs) = do
    ys <- permute xs
    k <- randomRIO (0,length ys)
    let (pre,post) = splitAt k ys
    return $ pre ++ x : post

mkJob :: JobControl -> Category Catinfo -> UTCTime -> Handler Job
mkJob config cat now = do
    let ci = contents cat 
        (+>) = T.append
    bs <- select_benchmarks (benchmarks_per_category config) $ benchmarks ci
    return $ Job 
         { postproc_id = postproc ci
         , description = repair $ categoryName cat
         , job_name = compact $ repair $ categoryName cat +> "@" +> T.pack (show now)
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

convertMC mc = S.MetaCategory (metaCategoryName mc)
         $ map convertC (categories mc)

convertC c = S.Category (categoryName c) [ S.YES, S.NO ] 
         $ let (_, jobs) = contents c in jobs

