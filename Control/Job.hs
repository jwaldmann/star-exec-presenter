-- | make job xmls from registration info.
-- for syntax of xml file, see https://github.com/stefanvonderkrone/star-exec-presenter/issues/18#issuecomment-46840686
-- for usage of hamlet, see http://www.yesodweb.com/book/shakespearean-templates

{-# LANGUAGE QuasiQuotes #-}

import StarExec.Registration

import qualified Data.Text as T
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Data.Time.Clock
import Prelude ( IO, show, unwords, putStrLn, ($), snd, Maybe (..), map, elem, take, reverse )
import qualified Data.Map.Strict as M

main :: Prelude.IO ()
main = do    
    now <- Data.Time.Clock.getCurrentTime 
    let ekat n xs = reverse $ take n $ reverse xs
    let -- forbidden chars must be replaces
        repair c = if c `elem` ":()-" then '.' else c
        -- jobname cannot be too long (64 chars)
        jobname me cat = map repair 
           $ unwords [ "termcomp", "auto"
                     , ekat 5 $ T.unpack $ metaCategoryName me
                     , ekat 5 $ T.unpack $ categoryName cat
                     , show now 
                     ]
    putStrLn "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    putStrLn $ renderHtml [shamlet|
    <tns:Jobs xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="https://www.starexec.org/starexec/public/batchJobSchema.xsd batchJobSchema.xsd" xmlns:tns="https://www.starexec.org/starexec/public/batchJobSchema.xsd">

      $forall mecat <- metacategories tc2014
        $forall cat <- categories mecat

          <Job cpu-timeout="240" description="no description" mem-limit="128.0" name="#{jobname mecat cat}" postproc-id="#{postproc (contents cat)}" queue-id="478" start-paused="false" wallclock-timeout="60">
              $forall p <- participants (contents cat)
                $forall b <- benchmarks (contents cat)
                  $maybe sc <- solver_config p
                    <JobPair bench-id="#{bench b}" config-id="#{snd sc}">
    |]
