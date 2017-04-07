{-# OPTIONS
 
 -XTypeSynonymInstances
 -XFlexibleInstances
 -XTypeFamilies
#-}

module GCJParse where

import TypeSyns
import Text.Parsec
import Text.Parsec.String

newtype Lines a = Lines [a]

-- data Header = None | Ignore | Read (Int -> Int)

data WithHeader a = WithHeader (Int -> Int) a

data IgnoreHeader a = IgnoreHeader a

type family ParseAs a :: * where
  ParseAs I = I
  ParseAs In = In
  ParseAs F = F
  ParseAs D = D
  ParseAs B = B
  ParseAs S = S
  ParseAs C = C
  ParseAs (a, b) = (ParseAs a, ParseAs b)
  ParseAs [a] = [ParseAs a]
  ParseAs (Lines a) = [ParseAs a]
  ParseAs (WithHeader a) = [ParseAs a]
  ParseAs (IgnoreHeader a) = ParseAs a


class GParseable a where
    gParser :: Parser (ParseAs a)

genWord :: Parser String
genWord = many1 (noneOf " (),\n")

line :: Parser String
line = many (noneOf "\n")


instance GParseable I where
    gParser = fmap read genWord

instance GParseable In where
    gParser = fmap read genWord

instance GParseable S where
    gParser = fmap read genWord

instance GParseable C where
    gParser = anyChar

instance GParseable F where
    gParser = fmap read genWord

instance GParseable D where
    gParser = fmap read genWord

instance GParseable B where
    gParser = fmap read genWord

instance (GParseable a, GParseable b) => GParseable (a,b) where
    gParser = do
      a <- gParser -- ::Parser (ParseAs a)
      spaces
      b <- gParser -- ::Parser (ParseAs b)
      return (a,b) -- ::Parser (ParseAs (a,b))

instance GParseable a => GParseable [a] where
    gParser = sepEndBy gParser spaces

instance GParseable a => GParseable (Lines a) where
    gParser = sepEndBy gParser newline

{-
gParse :: (GParseable a) => Header -> String -> a
gParse header str = do
    case header of
      None -> return ()
      _ -> line --eat a line
    sepEndBy 
      (do
-}
        
