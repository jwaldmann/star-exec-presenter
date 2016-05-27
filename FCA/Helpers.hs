module FCA.Helpers where

import Import

import Data.Maybe
import qualified Data.Set as Set
import Data.Text as T (append, pack, replace, unpack)
import Text.Regex.TDFA

-- https://github.com/nh2/haskell-ordnub#dont-use-nub
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- remove dash from given text
removeDash :: Text -> Text
removeDash = T.replace "-" ""

-- prefix given text with dash
dashPrefix :: Text -> Text
dashPrefix t = (T.pack "-") `T.append` t

-- trim after letters to retain solver basename
getSolverBasename :: Text -> Text
getSolverBasename = T.pack . matchBasename . T.unpack . removeDash

-- match basename from given solvername
matchBasename :: String -> String
matchBasename n = (n =~ ("[a-zA-Z]*" :: String) :: String)

maybeListId :: Maybe [a] -> [a]
maybeListId = fromMaybe []
