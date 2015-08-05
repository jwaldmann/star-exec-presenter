module Presenter.Output

( Output (..), showp
, (<+>), (<#>)
, text, braces, hsep, equals
)
       
where

import Prelude
import Data.String
import Text.PrettyPrint.Leijen as P hiding ((<$>), fill) 
import qualified Data.Text as T

class Output t where output :: t -> Doc
instance IsString Doc where fromString = text

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
p <#> q = fillBreak 4 p <+> q

showp :: Output a => a -> String
showp = ( \ d -> displayS d "" ) . renderPretty 1.0 80 . output

