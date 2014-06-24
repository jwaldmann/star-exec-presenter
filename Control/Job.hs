-- | make job xmls from registration info.
-- for syntax of xml file, see https://github.com/stefanvonderkrone/star-exec-presenter/issues/18#issuecomment-46840686
-- for usage of hamlet, see http://www.yesodweb.com/book/shakespearean-templates

{-# LANGUAGE QuasiQuotes #-}

import StarExec.Registration

import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Data.Time.Clock
import Prelude ( IO, show, unwords, putStrLn, ($), snd, Maybe (..), map )
import qualified Data.Map.Strict as M

main :: Prelude.IO ()
main = do    
    now <- Data.Time.Clock.getCurrentTime 
    let repair c = case c of ':' -> '.' ; _ -> c
        jobname = map repair $ unwords [ "termcomp", "auto", show now ]
    putStrLn "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
    putStrLn $ renderHtml [shamlet|
    <tns:Jobs xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="https://www.starexec.org/starexec/public/batchJobSchema.xsd batchJobSchema.xsd" xmlns:tns="https://www.starexec.org/starexec/public/batchJobSchema.xsd">
        <Job cpu-timeout="240" description="no description" mem-limit="128.0" name="#{jobname}" postproc-id="44" queue-id="478" start-paused="false" wallclock-timeout="60">
          $forall mecat <- metacategories tc2014
            $forall cat <- categories mecat
              $forall p <- participants (contents cat)
                $forall b <- benchmarks (contents cat)
                  $maybe sc <- solver_config p
                    <JobPair bench-id="#{bench b}" config-id="#{snd sc}">
    |]
