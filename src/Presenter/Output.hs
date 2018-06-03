module Presenter.Output

( Output (..)
, show_strict, show_lazy, show_string
, (<+>), (<#>)
, text, braces, hsep, equals
, dutch_record
)
       
where

import Prelude
import Data.String

import Data.Time.Clock
import Data.Time.Format

import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Render.Text as PRT
import qualified Data.Text.Prettyprint.Doc.Render.String as PRS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

type Doc = P.Doc ()

class Output t where output :: t -> Doc
-- instance IsString Doc where fromString = text

braces = P.braces
empty = mempty
vcat = P.vcat
align = P.align
(<+>) = (P.<+>)
fillBreak = P.fillBreak
(<>) = (P.<>)
text = fromString
list = P.list
hsep = P.hsep
equals = P.equals

instance Output () where 
    output = text . show

-- erstaunlich, daß das mit der Read-Instanz zusammen paßt.
-- wird in Presenter.Handler.CompetitionText benutzt
instance Output UTCTime where 
    output = text . formatTime defaultTimeLocale rfc822DateFormat 

instance Output Bool where 
    output = text . show
instance Output Int where 
    output = text . show
instance Output T.Text where
    output = text . show
instance Output t => Output [t] where 
    output = list . map output
instance Output t => Output (Maybe t) where
    output x = case x of
        Nothing -> "Nothing"
        Just a -> "Just" <+> align (output a)
instance (Output a, Output b) => Output (a,b) where
    output (x,y) = "(" <> output x <> "," <> output y <> ")"
instance (Output a, Output b,Output c) => Output (a,b,c) where
    output (x,y,z) = "(" <> output x <> "," <> output y <> "," <> output z <> ")"

(<#>) :: Doc -> Doc -> Doc
p <#> q = fillBreak 4 p <+> align q

show_string :: Output a => a -> ShowS
show_string = PRS.renderShowS .  P.layoutPretty P.defaultLayoutOptions . output

show_strict :: Output a => a -> T.Text
show_strict = PRT.renderStrict .  P.layoutPretty P.defaultLayoutOptions . output

show_lazy :: Output a => a -> TL.Text
show_lazy = PRT.renderLazy . P.layoutPretty P.defaultLayoutOptions . output

dutch_record :: [Doc] -> Doc
dutch_record [] = braces empty
dutch_record xs = vcat $ zipWith (<+>) ( "{" : repeat "," ) xs ++ [ "}" ]
