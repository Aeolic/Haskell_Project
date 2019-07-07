module ParserCon
-- | The following functions are exported:
  ( module Control.Applicative -- ^ This exports the operators (<*, *>, <*>, <|>,...)
    --   and combintors `empty' and `many'
  , module Control.Monad
  , ParserC       -- ^ the ParserC newtype
  , lit          -- ^ versions of old combinators that work with the newtype
  , satisfy
  , try
  , parse        -- ^ return (Just result) on success
  , parseAll     -- ^ returns the `list of successes' (just applies the underlying RawParserC)
  , parseLongest -- ^ returns the longest possible parse (as ParserCesult value)
  ) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Char

-- ^ Execution of a ParserC
parse :: ParserC t r -> [t] -> Maybe r
parse p ts = case  parseLongest p ts of
               Match x -> Just x
               _ -> Nothing

data ParserCesult t r = Match r | Partial (r, [t]) | Fail
  deriving (Show, Eq)

parseLongest :: ParserC t r -> [t] -> ParserCesult t r
parseLongest p ts =  maybe (tryPartial allResults) Match $  tryMatch allResults
   where allResults = parseAll p ts
         tryMatch rs = fmap fst $ listToMaybe $ dropWhile (not . null . snd) rs
         tryPartial [] = Fail
         tryPartial xs = Partial $ minimumBy (compare `on` (length . snd)) xs

parseAll :: ParserC t r -> [t] -> [(r, [t])]
parseAll p = rawParserC p

-- ^ RawParserC, Called "ParserC" in the course
type RawParserC token result = [token] -> [(result, [token])]

pempty :: RawParserC t r
pempty ts = []

psucceed :: r -> RawParserC t r
psucceed r ts = [(r, ts)]

psatisfy :: (t -> Bool) -> RawParserC t t
psatisfy p [] = []
psatisfy p (t:ts) | p t       = [(t, ts)]
                  | otherwise = []


msatisfy :: (t -> Maybe a) -> RawParserC t a
msatisfy f [] = []
msatisfy f (t:ts) = case f t of
                      Nothing -> []
                      Just a  -> psucceed a ts

plit :: Eq t => t -> RawParserC t t
plit t = psatisfy (== t)

palt :: RawParserC t r -> RawParserC t r -> RawParserC t r
palt p1 p2 = \ts -> p1 ts ++ p2 ts

pseq :: RawParserC t (s -> r) -> RawParserC t s -> RawParserC t r
pseq p1 p2 ts = [ (f s, ts'') | (f, ts') <- p1 ts, (s, ts'') <- p2 ts' ]

pmap :: (s -> r) -> RawParserC t s -> RawParserC t r
pmap f p ts = [ (f s, ts') | (s, ts') <- p ts]



-- ^ Modifications and instances, analogous to ParserC' in the course
newtype ParserC token result = P (RawParserC token result)

rawParserC :: ParserC t r -> RawParserC t r
rawParserC (P p) = p

instance Functor (ParserC t) where
  fmap f = P . pmap f . rawParserC

instance Applicative (ParserC t) where
  pure v = P $ psucceed v
  (P p1) <*> (P p2) = P (p1 `pseq` p2)

instance Alternative (ParserC t) where
  empty = P pempty
  P p1 <|> P p2 = P (p1 `palt` p2)

instance Monad (ParserC t) where
  return = pure
  (>>=) (P p) f = P $ \ts ->
    concatMap (\(r, rest) -> rawParserC (f r) rest) $ p ts

instance MonadPlus (ParserC t) where
  mzero = empty
  mplus = (<|>)

lit x = P $ plit x
satisfy p = P $ psatisfy p
try p = P $ msatisfy p

pmany :: ParserC t r -> ParserC t [r]
pmany p = ((:) <$> p <*> pmany p) <|> pure []

pmany1 :: ParserC t r -> ParserC t [r]
pmany1 p = (:) <$> p <*> pmany p

pIntersperse ::
    ParserC t r -> ParserC t w -> ParserC t [r] 
pIntersperse pThing pSep =
    (body <|> return [])
    where
      body = (:) <$> pThing <*> pmany p
      p = pSep *> pThing

pInt :: ParserC Char Integer
pInt = read <$> pmany1 (satisfy isDigit)

pIntList :: ParserC Char [Integer]
pIntList =
    lit '[' *> pIntersperse pInt (lit ',') <* lit ']'