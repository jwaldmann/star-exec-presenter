module Presenter.Output

( Output (..), showp
, (<+>), (<#>)
, text, braces, hsep, equals
, dutch_record
)
       
where

import Prelude
import Data.String

import Data.Time.Clock
import Data.Time.Format

import Text.PrettyPrint.Leijen as P hiding ((<$>), fill) 
import qualified Data.Text as T

class Output t where output :: t -> Doc
instance IsString Doc where fromString = text

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

showp :: Output a => a -> String
showp = ( \ d -> displayS d "" ) . renderPretty 1.0 80 . output

dutch_record :: [Doc] -> Doc
dutch_record [] = braces empty
dutch_record xs = vcat $ zipWith (<+>) ( "{" : repeat "," ) xs ++ [ "}" ]
