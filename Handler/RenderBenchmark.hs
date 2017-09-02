module Handler.RenderBenchmark where

import Import
import Data.Text.Encoding
import Presenter.StarExec.Commands (getBenchmark)
import qualified Data.ByteString.Lazy as BSL
import Handler.DisplayProof (with_prologue)
import Text.XML (Document(..), Instruction(..), Miscellaneous(..), Prologue(..))
import Control.Monad.Logger

getRenderBenchmarkR :: BenchmarkID ->  Handler TypedContent
getRenderBenchmarkR (StarExecBenchmarkID bmId) = do
  cont <- getBenchmark (error "con.8") bmId
  return $ with_prologue xtcHTML cont

xtcHTML = Prologue
    { prologueBefore =
        [MiscInstruction
         (Instruction { instructionTarget = "xml-stylesheet"
                      , instructionData = "type=\"text/xsl\" href=\"/static/xsl/xtcHTML.xsl\""})
        ]
    , prologueDoctype = Nothing, prologueAfter = []
    }

