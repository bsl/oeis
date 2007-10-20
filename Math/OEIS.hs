-- | Look up sequences in the Online Encyclopedia of Integer Sequences (OEIS).

module OEIS 
  ( 

    getSequenceByID, getSequenceByID_IO,
    extendSequence,

  ) where

import Network.HTTP
import Network.URI
import System.IO.Unsafe (unsafePerformIO)
import Data.List (intersperse, isPrefixOf, tails, foldl')
import Data.Char (toUpper, toLower)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Arrow

type SequenceData = [Integer]

-- | Look up a sequence in the OEIS by its catalog number.  Generally
-- this would be its A-number, but M-numbers (from the /Encyclopedia of
-- Integer Sequences/) and N-numbers (from the /Handbook of Integer
-- Sequences/) can be used as well.
--
-- Note that the result is not in the 'IO' monad, even though the
-- implementation requires looking up information via the
-- Internet. There are no side effects to speak of, and from a
-- practical point of view the function is referentially transparent
-- (OEIS A-numbers could change in theory, but it's extremely
-- unlikely).  If you're a nitpicky purist, feel free to use the
-- provided 'getSequenceByID_IO' instead.
-- 
-- Examples:
-- 
-- > *OEIS> getSequenceByID "A000040"    -- the prime numbers
-- > Just [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,...]
-- >
-- > *OEIS> getSequenceByID "A-1"        -- no such sequence!
-- > Nothing

getSequenceByID :: String -> Maybe SequenceData
getSequenceByID = unsafePerformIO . getSequenceByID_IO

-- | The same function as 'getSequenceByID', but with a result in the 'IO'
-- monad.
getSequenceByID_IO :: String -> IO (Maybe SequenceData)
getSequenceByID_IO x = lookupSequenceByID_IO x >>= return . fmap sequenceData

-- | Look up a sequence by ID number, returning a data structure
-- containing the entirety of the information the OEIS has on the
-- sequence.
--
-- Standard disclaimer about not being in the 'IO' monad applies.

lookupSequenceByID :: String -> Maybe OEISSequence
lookupSequenceByID = unsafePerformIO . lookupSequenceByID_IO

-- | Same as 'lookupSequenceByID', but in the 'IO' monad.
lookupSequenceByID_IO :: String -> IO (Maybe OEISSequence)
lookupSequenceByID_IO = getOEIS idSearchURI

-- | Extend a sequence by using it as a lookup to the OEIS, taking 
-- the first sequence returned as a result, and using it to augment 
-- the original sequence.
--
-- Note that xs is guaranteed to be a prefix of (extendSequence xs).
-- If the matched OEIS sequence contains any elements prior to those
-- matching xs, they will be dropped.  In addition, if no matching
-- sequences are found, xs will be returned unchanged.
--
-- The result is not in the IO monad even though the implementation
-- requires looking up information via the Internet.  There are no
-- side effects, and practically speaking this function is
-- referentially transparent (technically, results may change from
-- time to time when the OEIS database is updated; this is slightly
-- more likely than the results of getSequenceByID changing, but still
-- unlikely enough to be essentially a non-issue.  Again, purists may
-- use extendSequence_IO).
--
-- Examples:
--
-- TODO: put some examples here.

extendSequence :: [Integer] -> [Integer]
extendSequence = unsafePerformIO . extendSequence_IO

-- | The same function as extendSequence, but with a result in the IO
-- monad.
extendSequence_IO :: [Integer] -> IO [Integer]
extendSequence_IO [] = return []
extendSequence_IO xs = do oeis <- lookupSequence_IO xs
                          case oeis of 
                            Nothing -> return xs
                            Just s  -> return $ extend xs (sequenceData s)

lookupSequence :: [Integer] -> OEISSequence
lookupSequence = unsafePerformIO . lookupSequence_IO

lookupSequence_IO :: [Integer] -> IO OEISSequence
lookupSequence_IO = getOEIS seqSearchURI

-- | 'extend xs ext' returns the maximal suffix of ext of which xs is
-- a prefix, or xs if xs is not a prefix of any suffixes of ext. It
-- is guaranteed that
--
-- forall xs ext. xs `isPrefixOf` (extend xs ext)

extend :: [Integer] -> [Integer] -> [Integer]
extend xs ext = fromMaybe xs . listToMaybe . filter (xs `isPrefixOf`) $ tails ext

baseSearchURI = "http://www.research.att.com/~njas/sequences/?n=1&fmt=3&q="

idSearchURI :: String -> String
idSearchURI n = baseSearchURI ++ "id:" ++ n

seqSearchURI :: [Integer] -> String
seqSearchURI xs = baseSearchURI ++ (concat . intersperse "," . map show $ xs)

data LookupError = LookupError deriving Show

getOEIS :: (a -> String) -> a -> IO (Maybe OEISSequence)
getOEIS toURI key = case parseURI (toURI key) of
                      Nothing  -> return Nothing
                      Just uri -> do content <- get uri
                                     case content of
                                       (Left LookupError) -> return Nothing
                                       (Right text) -> return $ parseOEIS text

get :: URI -> IO (Either LookupError String)
get uri = do
    eresp <- simpleHTTP (request uri)
    case eresp of
      (Left e) -> return (Left LookupError)
      (Right resp) -> case rspCode resp of
                       (2,0,0) -> return (Right $ rspBody resp)
                       _ -> return (Left LookupError)

request :: URI -> Request
request uri = Request{ rqURI = uri,
                       rqMethod = GET,
                       rqHeaders = [],
                       rqBody = "" }

-----------------------------------------------------------

-- hmm... redo the parsing with Parsec?  seems like overkill.

type CatalogNumber = String
type Reference     = String
data Language = Mathematica | Maple | Other deriving Show
data Keyword = Base | Bref | Cofr | Cons | Core | Dead | Dumb | Dupe |
               Easy | Eigen | Fini | Frac | Full | Hard | More | Mult |
               New | Nice | Nonn | Obsc | Sign | Tabf | Tabl | Uned |
               Unkn | Walk | Word 
       deriving (Eq,Show,Read)

readKeyword :: String -> Keyword
readKeyword = read . capitalize

capitalize :: String -> String
capitalize ""     = ""
capitalize (c:cs) = toUpper c : map toLower cs

data OEISSequence = OEIS { catalogNums  :: [CatalogNumber], -- %I
                           sequenceData :: SequenceData,    -- %S,T,U
                           signedData   :: SequenceData,    -- %V,W,X
                           description  :: String,          -- %N
                           references   :: [String],        -- %D
                           links        :: [String],        -- %H
                           formulas     :: [String],        -- %F
                           xrefs        :: [String],        -- %Y
                           author       :: String,          -- %A
                           offset       :: Int,             -- %O
                           firstGT1     :: Int,             -- %O
                           programs     :: [(Language,String)],  -- %p,t,o
                           notes        :: [String],        -- %E
                           examples     :: [String],        -- %e
                           keywords     :: [Keyword],       -- %K
                           comments     :: [String]         -- %C
                         }  deriving Show

emptyOEIS :: OEISSequence
emptyOEIS = OEIS [] [] [] "" [] [] [] [] "" 0 0 [] [] [] [] []

addElement :: (Char, String) -> (OEISSequence -> OEISSequence)
addElement ('I', x) c = c { catalogNums = words x }
addElement (t, x)   c | t `elem` "STU" = c { sequenceData = nums ++ (sequenceData c) }
    where nums = map read $ csvItems x 
addElement (t, x)   c | t `elem` "VWX" = c { signedData = nums ++ (signedData c) }
    where nums = map read $ csvItems x
addElement ('N', x) c = c { description = x                  }
addElement ('D', x) c = c { references  = x : (references c) } 
addElement ('H', x) c = c { links       = x : (links c)      }
addElement ('F', x) c = c { formulas    = x : (formulas c)   }
addElement ('Y', x) c = c { xrefs       = x : (xrefs c)      }
addElement ('A', x) c = c { author      = x                  }
addElement ('O', x) c = c { offset      = read o             
                          , firstGT1    = read f }
  where (o,f) = second tail . splitWhile (/=',') $ x
addElement ('p', x) c = c { programs    = (Mathematica, x) :
                                            (programs c)     }
addElement ('t', x) c = c { programs    = (Maple, x) :
                                            (programs c)     }
addElement ('o', x) c = c { programs    = (Other, x) : 
                                            (programs c)     }
addElement ('E', x) c = c { notes       = x : (notes c)      }
addElement ('e', x) c = c { examples    = x : (examples c)   }
addElement ('K', x) c = c { keywords    = parseKeywords x    }
addElement ('C', x) c = c { comments    = x : (comments c)   }

parseOEIS :: String -> Maybe OEISSequence
parseOEIS x = if "no match" `isPrefixOf` (ls!!1) 
                then Nothing
                else Just . foldl' (flip addElement) emptyOEIS . reverse . parseRawOEIS $ ls'
    where ls = lines x
          ls' = init . drop 3 $ ls

parseRawOEIS :: [String] -> [(Char, String)]
parseRawOEIS = map parseItem . combineConts

parseKeywords :: String -> [Keyword]
parseKeywords = map readKeyword . csvItems

csvItems :: String -> [String]
csvItems "" = []
csvItems x = item : others
    where (item, rest) = splitWhile (/=',') x
          others = csvItems $ del ',' rest

del :: Char -> String -> String
del _ ""     = ""
del c (x:xs) | c==x      = xs
             | otherwise = (x:xs)

parseItem :: String -> (Char, String)
parseItem s = (c, str) 
    where ( '%':c:_ , rest) = splitWord s
          ( idNum, str )    = if (c == 'I') then ("", rest)
                                            else splitWord rest
                           
combineConts :: [String] -> [String]
combineConts [] = []
combineConts [x] = [x]
combineConts (s@('%':c:_) : ss) = 
  uncurry (:) . (joinConts s *** combineConts) . splitConts $ ss

splitWord :: String -> (String, String)
splitWord = second trimLeft . splitWhile (/= ' ')

splitConts :: [String] -> ([String], [String])
splitConts = splitWhile (not . isItem)

isItem :: String -> Bool
isItem x = not (null x) && '%' == head x

joinConts :: String -> [String] -> String
joinConts s conts = s ++ (concat . map trimLeft $ conts)

trimLeft :: String -> String
trimLeft = dropWhile (== ' ')

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile _ []     = ([],[])
splitWhile p xxs@(x:xs) | p x       = first (x:) (splitWhile p xs)
                        | otherwise = ([], xxs)

