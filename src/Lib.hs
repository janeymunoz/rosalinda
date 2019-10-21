module Lib where

import Protolude
import Prelude (tail, (!!), init)
import qualified Data.Map.Strict as DMS
import qualified Data.Set as DS
import qualified Data.Text as DT

--------------------------------------------------------------------------------
-- Introduction to the Bioinformatics Armory (ini)
--------------------------------------------------------------------------------

data BasesCount = BasesCount Int Int Int Int
  deriving (Show)

bases0 :: BasesCount
bases0 = BasesCount 0 0 0 0

ini :: [Char] -> BasesCount
ini t = foldr accum bases0 t
  where accum :: Char -> BasesCount -> BasesCount
        accum char bs@(BasesCount a c g t) =
          case char of
            'A' -> BasesCount (a+1) c g t
            'C' -> BasesCount a (c+1) g t
            'G' -> BasesCount a c (g+1) t
            'T' -> BasesCount a c g (t+1)
            _   -> bs

--------------------------------------------------------------------------------
-- Fibonacci Numbers (fibo)
--------------------------------------------------------------------------------

fibo :: Int -> Int
fibo nth
  | nth <= 0 = 0
  | nth == 1 = 1
  | otherwise = (fibo (nth-1)) + (fibo (nth-2))

fibs = 0 : 1 : zipWith (+) fibs (Prelude.tail fibs)
fibsSol n = fibs !! n

fiboParse :: Text -> Int
fiboParse = fromMaybe 0 . head . parseInts . DT.lines

--------------------------------------------------------------------------------
-- Degree Array (deg)
--------------------------------------------------------------------------------

deg :: [(Int,Int)] -> Map Int Int
deg edgesAll = snd $ foldr accum (mempty, mempty) edgesAll
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
degParse t = map (bimap (fromMaybe 0 . parseInt)
                                    (fromMaybe 0 . parseInt))
                      $ map (DT.breakOn space) . Prelude.tail $ DT.lines t
  where space :: Text
        space = DT.pack " "

-- Output should be of form "v1EdgeCount v2EdgeCount v3EdgeCount"
-- ex: "20 23 42 32"
degLex :: Map Int Int -> Text
degLex m = DT.unwords . map toS $ foldr accum mempty $ DMS.toAscList m
  where accum :: (Int, Int) -> [[Char]] -> [[Char]]
        accum (_, d) cs = show d : cs

--------------------------------------------------------------------------------
-- Mortal Fibonacci Rabbits (fibd)
--------------------------------------------------------------------------------

type Age = Int
newtype RabbitPair = RabbitPair Age

-- Input of form "Int Int"
-- ex: "6 3" ("n, m")
fibdParse :: Text -> (Int, Int)
fibdParse t = (listInts !! 0, listInts !! 1)
  where listInts = parseInts . concatMap DT.words $ DT.lines t

-- Prohibitively slow version
fibd' :: (Int, Int) -> Int
fibd' (months, lifespan) =
  length $
    foldr accum
      [RabbitPair 0]
      [1 .. (months-1)]
  where accum :: Int -> [RabbitPair] -> [RabbitPair]
        accum _ rps = fst ls <> (catMaybes $ snd ls)
          where ls = unzip $ map (incrRabbit lifespan) rps
		incrRabbit :: Int
                           -> RabbitPair
			   -> (RabbitPair, Maybe RabbitPair)
		incrRabbit lifespan (RabbitPair age)
		  | age == (lifespan-1) = (rabbitNew, Nothing)
		  | age >= maturity = (rabbitExist, Just rabbitNew)
		  | otherwise = (rabbitExist, Nothing)
		  where incrAge = age + 1
			rabbitExist = RabbitPair incrAge
			rabbitNew = RabbitPair 0
			maturity = 1

-- Fast version!
fibd :: (Int, Int) -> Integer
fibd (months, lifespan) = 
    sum $ foldl' accum initLiveRabbits [1.. (months -1)]
  where
    initLiveRabbits = 1 : replicate (lifespan - 1) 0

    shiftRabbits gen rabbits = gen : Prelude.init rabbits

    accum :: [Integer] -> Int -> [Integer]
    accum liveRabbits _ = newRabbits
      where
        reproRabbits = Prelude.tail liveRabbits
        numNewRabbits = sum reproRabbits
        newRabbits = shiftRabbits numNewRabbits liveRabbits


-- Output of form "Int"
-- ex: "4"
fibdLex :: Integer -> Text
fibdLex = DT.pack . show

--------------------------------------------------------------------------------
-- Genome Assembly as Shortest Superstring (long)
--------------------------------------------------------------------------------

data Base = A | C | G | T
  deriving (Eq, Ord, Show)

longParse :: Text -> Set (FASTAID, [Base])
longParse = parseFASTA

long :: Set (FASTAID, [Base]) -> [Base]
long = undefined
  where overlaps :: [Base] -> [Base] -> Maybe [Base]
        overlaps s1 s2 = undefined
          where lenS1 = length s1
                half = (+) 1 $ div lenS1 2


longLex :: [Base] -> Text
longLex = basesToText

--------------------------------------------------------------------------------
-- General helper functions
--------------------------------------------------------------------------------

parseInts :: [Text] -> [Int]
parseInts = mapMaybe parseInt

parseInt :: Text -> Maybe Int
parseInt = readMaybe . DT.unpack

newtype FASTAID = FASTAID Text
  deriving (Eq, Ord, Show)

-- Given input Text of style
--   >FASTA_ID_X
--   >AA
--   >GGG
--   >FASTAID_Y
--   >TTT
--   >UUU
-- Will output a Set of Tuples, each tuple containing a FASTA ID associated
-- with a list of Bases.
--   fromList [(FASTAID "FASTA_ID_X", [A,A,G,G,G]), (FASTAID "FASTA_ID_Y",
--   [T,T,T,U,U,U])]
-- TODO: parts of the list handling would be faster if done in reverse
parseFASTA :: Text -> Set (FASTAID, [Base])
parseFASTA t = fourth $
                 foldl accum (length ts, FASTAID DT.empty, [], mempty) ts
  where ts = DT.lines t
        fourth (a, b, c, d) = d
        accum :: (Int, FASTAID, [Base], Set (FASTAID, [Base]))
              -> Text
              -> (Int, FASTAID, [Base], Set (FASTAID, [Base]))
        accum (lineID, fid@(FASTAID f), bs, s) t =
          case DT.uncons t of
            Just ('>', fidNew) ->
              if f == DT.empty
                 then (newLine, FASTAID fidNew, [], s)
                 else (newLine, FASTAID fidNew, [], DS.insert (fid, bs) s)
            Just _             ->
              if lineID > 1
                 then (newLine, fid, addOnBases, s)
                 else (newLine, fid, [], DS.insert (fid, addOnBases) s)
            Nothing            -> (newLine, fid, bs, s)
          where newLine = lineID - 1
                addOnBases = bs <> (DT.foldl charsToBases [] t)


charsToBases :: [Base] -> Char -> [Base]
charsToBases bs c =
  bs <> (mapMaybe charToBase [c])

charToBase :: Char -> Maybe Base
charToBase c =
  case c of
    'A' -> Just A
    'a' -> Just A
    'C' -> Just C
    'c' -> Just C
    'G' -> Just G
    'g' -> Just G
    'T' -> Just T
    't' -> Just T
    _   -> Nothing

basesToText :: [Base] -> Text
basesToText bs = DT.pack $ map baseToChar bs

baseToChar :: Base -> Char
baseToChar b =
  case b of
    A -> 'A'
    C -> 'C'
    G -> 'G'
    T -> 'T'
