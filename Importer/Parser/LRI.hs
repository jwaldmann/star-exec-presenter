module Importer.Parser.LRI 
  ( parse
  ) where

import Prelude
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as Char8
import Debug.Trace
import qualified Data.List as L
import Importer.Internal

trimTrailingSpace :: Line -> Line
trimTrailingSpace = Char8.unwords . filterEmpty . Char8.words
  where
    filterEmpty = filter (not . Char8.null)

getEntries :: [Line] -> Either String [Entry]
getEntries ls = parseLines ls (None, [])
  where
    parseLines :: [Line] -> (ParseState, [Entry]) -> Either String [Entry]
    -- no more lines, parsing finished
    parseLines [] (_, entries) = return entries
    -- parse lines
    parseLines (l:ls) s@(state, entries) =
      case state of
        -- new entry
        None -> do
          name <- getName l
          parseLines ls (Unfinished name [], entries)
        -- unfinished Entry
        p@(Unfinished n c) ->
          case l of
            "{" -> do
              let kvs = takeWhile (/= "}") ls
                  rest = drop (1 + length kvs) ls
              keyValuePairs <- parseKeyValues kvs
              parseLines rest (None, (n, keyValuePairs) : entries)
            _ -> Left $ "unexpected line: " ++ (show l)
    -- extract name for new entry
    getName :: Line -> Either String Name
    getName bs = case Char8.words bs of
                   (n:_) -> return n
                   []    -> Left "no name found"
    -- parse key-value-pair
    parseKeyValues :: [Line] -> Either String Content
    parseKeyValues [] = return []
    parseKeyValues (l:ls) =
      case Char8.words l of
        -- simpliest case
        (k:"=":v:";":[]) -> do
          kvs <- parseKeyValues ls
          return $ (k,v) : kvs
        -- watch next lines for a list or text
        (k:"=":[]) -> do
          (v, rest) <- getValue ls
          kvs <- parseKeyValues rest
          return $ (k,v) : kvs
        -- v is a list or text
        (k:"=":vs) -> do
          let line = Char8.unwords vs
          (v, rest) <- getValue (line:ls)
          kvs <- parseKeyValues rest
          return $ (k,v) : kvs
    getValue :: [Line] -> Either String (Value, [Line])
    getValue [] = Left "getValue: no more lines"
    getValue ("[] ;":ls) = return ("[]", ls)
    getValue vs@(l:ls)
      | "[ " `BSL.isPrefixOf` l = getValue' "] ;" vs
      | "\"" `BSL.isPrefixOf` l = getValue' "\" ;" vs
      | otherwise = Left $ "donno: " ++ (show l) ++ " next left: " ++ (show $ take 5 vs) -- get text
      where
        getValue' :: BSL.ByteString -> [Line] -> Either String (Value, [Line])
        getValue' s (l:[]) = return (l,[])
        getValue' s (l:ls) =
          if s `BSL.isSuffixOf` l
            then return (Char8.take (BSL.length l - 2) l,ls)
            else do
              (v, rest) <- getValue' s ls
              return (Char8.unwords (l:v:[]), rest)

parse :: BSL.ByteString -> Either String [Entry]
parse bs = do
  let ls = map trimTrailingSpace $ Char8.lines bs
  getEntries ls
