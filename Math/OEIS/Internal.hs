-- | This module exists just to facilitate testing.
-- /Nothing here is part of the OEIS API./

module Math.OEIS.Internal where

--------------------------------------------------------------------------------

import Control.Arrow (second, (***))
import Data.Char     (isSpace, toUpper, toLower)
import Data.List     (intercalate, isPrefixOf, foldl')
import Network.HTTP  (simpleHTTP, rspBody, rspCode, rqBody, rqHeaders, rqMethod, rqURI, Request(..), RequestMethod(GET))
import Network.URI   (parseURI, URI)

import Math.OEIS.Types

--------------------------------------------------------------------------------

baseSearchURI :: String
baseSearchURI = "http://oeis.org/search?fmt=text&q="

idSearchURI :: String -> String
idSearchURI n = baseSearchURI ++ "id:" ++ n

seqSearchURI :: SequenceData -> String
seqSearchURI xs = baseSearchURI ++ intercalate "," (map show xs)

getOEIS :: (a -> String) -> a -> IO (Maybe OEISSequence)
getOEIS toURI key =
    case parseURI (toURI key) of
      Nothing  -> return Nothing
      Just uri -> do
          mbody <- get uri
          return $ case mbody of
            Nothing   -> Nothing
            Just body -> parseOEIS body

get :: URI -> IO (Maybe String)
get uri = do
    ersp <- simpleHTTP (request uri)
    return $ case ersp of
      Left _ -> Nothing
      Right rsp
        | rspCode rsp == (2,0,0) -> Just $ rspBody rsp
        | otherwise              -> Nothing

request :: URI -> Request String
request uri = Request
  { rqURI     = uri
  , rqMethod  = GET
  , rqHeaders = []
  , rqBody    = ""
  }

readKeyword :: String -> Keyword
readKeyword = read . capitalize

capitalize :: String -> String
capitalize ""     = ""
capitalize (c:cs) = toUpper c : map toLower cs

emptyOEIS :: OEISSequence
emptyOEIS = OEIS [] [] [] "" [] [] [] [] "" 0 0 [] [] [] [] []

addElement :: (Char, String) -> OEISSequence -> OEISSequence
addElement ('I', x) c = c { catalogNums = words x }
addElement (t, x)   c | t `elem` "STU" = c { sequenceData = nums ++ sequenceData c }
    where nums = map read $ csvItems x
addElement (t, x)   c | t `elem` "VWX" = c { signedData = nums ++ signedData c }
    where nums = map read $ csvItems x
addElement ('N', x) c = c { description = x                  }
addElement ('D', x) c = c { references  = x : references c }
addElement ('H', x) c = c { links       = x : links c      }
addElement ('F', x) c = c { formulas    = x : formulas c   }
addElement ('Y', x) c = c { xrefs       = x : xrefs c      }
addElement ('A', x) c = c { author      = x                  }
addElement ('O', x) c = c { offset      = read o
                          , firstGT1    = read f }
  where (o,f) = second tail . span (/=',') $ x
addElement ('p', x) c = c { programs    = (Maple, x) :
                                            programs c     }
addElement ('t', x) c = c { programs    = (Mathematica, x) :
                                            programs c     }
addElement ('o', x) c = c { programs    = (Other, x) :
                                            programs c     }
addElement ('E', x) c = c { extensions  = x : extensions c }
addElement ('e', x) c = c { examples    = x : examples c   }
addElement ('K', x) c = c { keywords    = parseKeywords x    }
addElement ('C', x) c = c { comments    = x : comments c   }
addElement _ c = c

parseOEIS :: String -> Maybe OEISSequence
parseOEIS x = if "No results." `isPrefixOf` (ls!!3)
                then Nothing
                else Just . foldl' (flip addElement) emptyOEIS . reverse . parseRawOEIS $ ls'
    where ls = lines x
          ls' = init . drop 5 $ ls

parseRawOEIS :: [String] -> [(Char, String)]
parseRawOEIS = map parseItem . combineConts

parseKeywords :: String -> [Keyword]
parseKeywords = map readKeyword . csvItems

csvItems :: String -> [String]
csvItems "" = []
csvItems x = item : others
    where (item, rest) = span (/=',') x
          others = csvItems $ del ',' rest

del :: Char -> String -> String
del _ ""     = ""
del c (x:xs) | c==x      = xs
             | otherwise = x:xs

parseItem :: String -> (Char, String)
parseItem s = (c, str)
    where ( '%':c:_ , rest) = splitWord s
          (_, str )    = if c == 'I' then ("", rest)
                                     else splitWord rest

combineConts :: [String] -> [String]
combineConts (s@('%':_:_) : ss) =
  uncurry (:) . (joinConts s *** combineConts) . break isItem $ ss
combineConts ss = ss

splitWord :: String -> (String, String)
splitWord = second trimLeft . break isSpace

isItem :: String -> Bool
isItem x = not (null x) && '%' == head x

joinConts :: String -> [String] -> String
joinConts s conts = s ++ concatMap trimLeft conts

trimLeft :: String -> String
trimLeft = dropWhile isSpace
