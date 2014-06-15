module Table.Query where

import Yesod
import Prelude


import qualified Data.Text as T

data Query = Query [ Transform ]
    deriving (Read,Show, Eq)

data Transform = Choose_Columns [ Int ]
               | Filter_Rows Predicate 
    deriving (Read,Show, Eq)               

data Predicate = And [ Cell_Filter ]
               | Not Predicate
    deriving (Read,Show, Eq)               

data Cell_Filter = Any 
                 | Equals T.Text 
                 | Not_Equals T.Text 
    deriving (Read,Show, Eq)


instance PathPiece Query where
    fromPathPiece t = case reads (unquote t) of
        [ (q, "") ] -> return q
        _ -> Nothing
    toPathPiece q = quote $ show q


-- FIXME: 
-- note: cannot use '-', since getClass returns "solver-certified" etc.
quote :: String -> T.Text
quote s = T.pack $ map ( \ c -> case c of
         ' ' -> '='
         '"' -> '+'
         _   -> c  ) s

unquote :: T.Text -> String
unquote t = map ( \ c -> case c of
         '=' -> ' '
         '+' -> '"'
         _ -> c ) $ T.unpack t
