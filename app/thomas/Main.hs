module Main where

import Prelude hiding (foldl')
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.FilePath (FilePath)

main :: IO ()
main = getArgs >>= \(fp:_) -> do
  input <- readInputFromFile fp
  print (completeATree input)

data Input = Input Int [(Int, Int)]

readInputFromFile :: FilePath -> IO Input
readInputFromFile fp = do
  fileLines <- lines <$> readFile fp
  case fileLines of
    [] -> error "uh oh"
    (nStr:edgeStrs) -> do
      let n = read nStr
          parseEdge l =
            case words l of
              [l,r] -> (read l, read r)
              _ -> error "uh oh"
          edges = map parseEdge edgeStrs
      pure (Input n edges)

type Output = Int

completeATree :: Input -> Output
completeATree (Input n edges) = Set.size allSubgraphs + Set.size remNodes - 1
  where
    unused = Set.fromList [1..n]

    buildSubgraphs :: Set (Set Int) -> (Int, Int) -> Set (Set Int)
    buildSubgraphs subgraphs edge@(l,r)
      | edgeInserted = subgraphs'
      | otherwise = Set.insert (Set.fromList [l,r]) subgraphs
      where
        (subgraphs', edgeInserted) =
          foldl' (insertEdge edge) (mempty, False) subgraphs

        insertEdge :: (Int, Int) -> (Set (Set Int), Bool) -> Set Int -> (Set (Set Int), Bool)
        insertEdge (l,r) (accumSet, done) set
          | l `Set.member` set = (Set.insert (Set.insert r set) accumSet, True)
          | r `Set.member` set = (Set.insert (Set.insert l set) accumSet, True)
          | otherwise = (Set.insert set accumSet, done || False)

    allSubgraphs = foldl' buildSubgraphs mempty edges
    remNodes = Set.fromList [1..n] `Set.difference` (Set.unions allSubgraphs)
