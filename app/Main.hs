module Main where

import Lib
import Protolude as Proto
import System.FilePath as FP
import System.Environment as SE
import Data.Map.Strict as DMS
import Data.Text as DT

main :: IO ()
main = do
  [problemID] <- SE.getArgs
  inputDataSet <- readFile $ fp problemID
  let answer = case DMS.lookup problemID problemIDs of
                 Nothing -> mempty
                 Just f  -> f inputDataSet
  putStrLn answer

problemIDs :: Map [Char] (Text -> Text)
problemIDs =
  DMS.fromList [ ( "ini", show . ini . toS )
               , ( "fibo", show . fibo . fromMaybe 0 . Proto.head . parseInts . DT.lines)
               , ( "deg", show . degLex . deg . degParse)
               , ( "fibd", show . fibdLex . fibd . fibdParse)
               ]

fp :: [Char] -> FilePath
fp problemID =
  "data" FP.</> "rosalind_" <> problemID <> ".txt"

