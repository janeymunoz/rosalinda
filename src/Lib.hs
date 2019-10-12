module Lib where

import Protolude as Proto
import Prelude (tail, (!!))
import Data.Map.Strict as DMS
import Data.Set as DS
import Data.Text as DT

--------------------------------------------------------------------------------
-- Introduction to the Bioinformatics Armory (ini)
--------------------------------------------------------------------------------

newtype A = A Int
  deriving (Show)
newtype T = T Int
  deriving (Show)
newtype G = G Int
  deriving (Show)
newtype C = C Int
  deriving (Show)

data BasesCount =
  BasesCount { a :: A
             , c :: C
             , g :: G
             , t :: T
             }
             deriving (Show)

bases0 :: BasesCount
bases0 = BasesCount {a = A 0, c = C 0, g = G 0, t = T 0}

ini :: [Char] -> BasesCount
ini t = Proto.foldr accum bases0 t
    where accum :: Char -> BasesCount -> BasesCount
          accum c bs@(BasesCount {a = A iA, c = C iC, g = G iG, t = T iT }) =
            case c of
              'A' -> bs { a = A $ iA + 1 }
              'C' -> bs { c = C $ iC + 1 } 
              'G' -> bs { g = G $ iG + 1 } 
              'T' -> bs { t = T $ iT + 1 }
              _   -> bs
--type BasesCountMap = Map Char Int
--foldr (\c accum -> insertWith (+) c 1 accum) mempty initialText

--------------------------------------------------------------------------------
-- Fibonacci Numbers (fibo)
--------------------------------------------------------------------------------

fibo :: Int -> Int
fibo nth
  | nth <= 0 = 0
  | nth == 1 = 1
  | otherwise = (fibo (nth-1)) + (fibo (nth-2))

fibs = 0 : 1 : Proto.zipWith (+) fibs (Prelude.tail fibs)
fibsSol n = fibs !! n

--------------------------------------------------------------------------------
-- Degree Array (deg)
--------------------------------------------------------------------------------

deg :: [(Int,Int)] -> Map Int Int
deg edgesAll = snd $ Proto.foldr accum (mempty, mempty) edgesAll
  where accum :: (Int,Int)
              -> (Set (Set Int), Map Int Int)
              -> (Set (Set Int), Map Int Int)
        accum (v1, v2) (s, m)
          | v1 == v2 = (s, m)
          | DS.member es s = (s, m)
          | otherwise =
              ( DS.insert es s
              , DMS.insertWith (+) v2 1 $ DMS.insertWith (+) v1 1 m
              )
          where es = DS.fromList [v1, v2]

degParse :: Text -> [(Int, Int)]
degParse t = Proto.map (Proto.bimap (fromMaybe 0 . parseInt)
                                    (fromMaybe 0 . parseInt))
                      $ Proto.map (DT.breakOn space) . Prelude.tail $ DT.lines t
  where space :: Text
        space = DT.pack " "

-- Output should be of form "v1EdgeCount v2EdgeCount v3EdgeCount"
-- ex: "20 23 42 32"
degLex :: Map Int Int -> Text
degLex m = unwords . Proto.map toS $ Proto.foldr accum mempty $ DMS.toAscList m
  where accum :: (Int, Int) -> [[Char]] -> [[Char]]
        accum (_, d) cs = show d : cs

--------------------------------------------------------------------------------
-- General helper functions
--------------------------------------------------------------------------------

parseInts :: [Text] -> [Int]
parseInts = Proto.mapMaybe parseInt

parseInt :: Text -> Maybe Int
parseInt = readMaybe . DT.unpack

