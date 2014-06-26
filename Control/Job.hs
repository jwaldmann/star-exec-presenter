-- | make job xmls from registration info.
-- for syntax of xml file, see https://github.com/stefanvonderkrone/star-exec-presenter/issues/18#issuecomment-46840686
-- for usage of hamlet, see http://www.yesodweb.com/book/shakespearean-templates

{-# LANGUAGE QuasiQuotes #-}

import StarExec.Registration

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import Text.Hamlet.XML
import Text.XML
import Data.Hashable


-- import Text.Blaze.Html.Renderer.String (renderHtml)

import Data.Time.Clock
import Prelude ( IO, show, unwords, putStrLn, print, ($), snd, Maybe (..), map, elem, take, reverse, sum, fromEnum, foldr, (+), (*), Bool (..), any, (.), maybe, const, otherwise )
import qualified Data.Map.Strict as M

main :: Prelude.IO ()
main = do    
    now <- Data.Time.Clock.getCurrentTime 
    let ekat n xs = reverse $ take n $ reverse xs
    let -- forbidden chars must be replaced
        repair c = if c `elem` ":()-" then '.' else c
        -- jobname cannot be too long (64 chars)
        jobname me cat = T.pack $ show $ hash
           $ unwords [ "termcomp", "auto"
                     , T.unpack $ metaCategoryName me
                     , T.unpack $ categoryName cat
                     , show now 
                     ]
    let nonemptyC = any isJust . map solver_config . participants . contents
        isJust = maybe False (const True)
    let doc = Document (Prologue [] Nothing []) root []
        root = Element "tns:Jobs" 
             (M.fromList [("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                         ,("xsi:schemaLocation", "https://www.starexec.org/starexec/public/batchJobSchema.xsd batchJobSchema.xsd")
                         ,("xmlns:tns","https://www.starexec.org/starexec/public/batchJobSchema.xsd") ]) [xml|
      $forall mecat <- metacategories tc2014
        $forall cat <- categories mecat
          $if nonemptyC cat
            <Job cpu-timeout="240" description="no description" mem-limit="128.0" name="#{jobname mecat cat}" postproc-id="#{T.pack $ show $ postproc $ contents cat}" queue-id="478" start-paused="false" wallclock-timeout="60">
              $forall p <- participants (contents cat)
                $forall b <- benchmarks (contents cat)
                  $maybe sc <- solver_config p
                    <JobPair bench-id="#{T.pack $ show $ bench b}" config-id="#{T.pack $ show $ snd sc}">
    |]
    T.putStrLn $ renderText def { rsPretty=True} doc 
