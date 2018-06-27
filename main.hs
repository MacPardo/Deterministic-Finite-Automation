import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Control.Applicative as Ap
import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.Maybe (isJust, fromJust)

data TerminalSymbol = TerminalSymbol Char
                    | Epsilon
                    deriving (Show, Eq, Ord)
type NonTerminalSymbol = String
type Symbol = Either TerminalSymbol NonTerminalSymbol
type Rule = (NonTerminalSymbol, [Symbol])

data State = State (String, Int, Bool)
           | FinalState Int
           | ErrorState
           deriving (Show, Eq, Ord)

type NDFA = M.Map State (M.Map TerminalSymbol (S.Set State))
type DFA =  M.Map (S.Set State) (M.Map TerminalSymbol (S.Set State))
{-
No DFA um conjunto de estados é tratado como se fosse um estado só
-}

epsilonRepresentation = "_E_"

splitAfter :: (Eq a) => a -> [a] -> [[a]]
splitAfter _ [] = []
splitAfter c s = case dropWhile (/= c) s of
                    [] -> first : []
                    (x:xs) ->  (first ++ [x]) : (splitAfter c xs)
  where first = takeWhile (/= c) s

splitBefore :: (Eq a) => a -> [a] -> [[a]]
splitBefore _ [] = []
splitBefore c s = case dropWhile (/= c) s of
                    [] -> first : []
                    (x:xs) ->  first : (case splitBefore c xs of
                                          [] -> [[x]]
                                          (xs:ys) -> ([x] ++ xs):ys)
  where first = takeWhile (/= c) s

separateSymbols :: String -> [String]
separateSymbols = filter (not . null) .
                  concat .
                  map (\x -> if head x == '<'
                             then [x]
                             else Split.splitOn "" x) .
                  filter (not . null) .
                  concat .
                  map (splitBefore '<') .
                  splitAfter '>'

stringToSymbol :: String -> Symbol
stringToSymbol (x:[]) = Left $ TerminalSymbol x
stringToSymbol (a:ys@(b:c:xs))
  | a == '<' && last ys == '>' = Right $ init ys
stringToSymbol _ = undefined

stringIsRule :: String -> Bool
stringIsRule = (" ::= " `List.isInfixOf`)

stringToRules :: String -> S.Set Rule
stringToRules s = S.fromList $
                  map (((,) ruleHeadState) .
                       map stringToSymbol .
                       separateSymbols)
                  rulesBodies
  where ruleHeadState = case stringToSymbol ruleHead of
                          (Right a) -> a
        rulesBodies = Split.splitOn " | " ruleBody
        ruleBody = splitArray !! 1
        ruleHead = splitArray !! 0
        splitArray = Split.splitOn " ::= " s

tokenStringToRules :: String -> S.Set Rule
tokenStringToRules s = S.fromList $ Ap.getZipList $
                       (,) <$> z rules <*> z bodies
  where bodies = (Ap.getZipList $ (\a b -> [a, Right b]) <$>
                   z (init symbols) <*>
                   z (tail rules))
                 ++ [[last symbols]]
        symbols = map stringToSymbol $ separateSymbols s
        rules = map show [1..]
        z = Ap.ZipList

mergeUniqueRules :: [S.Set Rule] -> S.Set (State, [Either TerminalSymbol State])
mergeUniqueRules rs = S.unions $ gz $ f <$> z [1..] <*> z rs
  where f p ruleSet = S.map g ruleSet
          where prefixBody (Right nts) = Right $ State (nts, p, False)
                prefixBody (Left ts) = Left ts
                g (ts, a) = (State (ts, p, False), map prefixBody a)
        z = Ap.ZipList
        gz = Ap.getZipList

mapFromTupleSet :: (Ord t, Ord k) => S.Set (t, k) -> M.Map t (S.Set k)
mapFromTupleSet ruleSet = S.foldr' f M.empty ruleSet
  where f (state, symbols) acc = M.insert state newSymbols acc
          where newSymbols = case ms of
                               Nothing -> S.singleton symbols
                               Just s  -> s `S.union` (S.singleton symbols)
                ms = M.lookup state acc

makeNDFA :: M.Map State (S.Set [Either TerminalSymbol State]) -> NDFA
makeNDFA m = M.map (mapFromTupleSet . S.fromList) .
             M.mapWithKey ((\k l -> map (f k) l)) .
             M.map S.toList $
             m
  where f (State (s, n, qf)) (Left a:[]) = (a      , FinalState n)
        f key (Left a:Right b:[])        = (a      , b)
        f key (Right b:[])               = (Epsilon, b)
        f _   _ = error "invalid rule in makeNDFA"

hasEpsilonTransitions :: NDFA -> Bool
hasEpsilonTransitions = M.foldr aux False
  where aux _ True  = True
        aux m False = case M.lookup Epsilon m >>=
                           (\s -> Just $ S.null s) of
                        Nothing -> False
                        Just  a -> a


removeEpsilonTransitions :: NDFA -> NDFA
removeEpsilonTransitions a
  |hasEpsilonTransitions a = M.map f a
  |otherwise = a
--  where f b = let et = 
  where f = undefined
               

mapOfSetsUnions :: (Ord a, Ord b) => [M.Map a (S.Set b)] -> M.Map a (S.Set b)
mapOfSetsUnions a = foldr f M.empty a 
  where f m acc = M.union (M.mapWithKey mergeValues acc) m
          where mergeValues k v
                  |isJust mRules = S.union (fromJust mRules) v
                  |otherwise = v
                  where mRules = M.lookup k m
  
--determinize :: NDFA -> DFA
determinize a = let stateSets = concat $ map M.elems $ M.elems a
                    test = map f stateSets
  in test

  where f set = S.map fromJust $ S.filter isJust $ S.map aux set
        aux state = M.lookup state a >>=
                    (\map -> Just $ M.assocs map) >>=
                    (\assocs -> Just $ S.fromList assocs) >>=
                    (\tupleSet -> Just $ mapFromTupleSet tupleSet) >>=
                    (\map -> Just $ M.map (S.unions . S.toList) map)

state2string :: State -> String
state2string (State s) = show s
state2string s = show s

terminalSymbol2string :: TerminalSymbol -> String
terminalSymbol2string Epsilon = epsilonRepresentation
terminalSymbol2string (TerminalSymbol t) = t:[]



main :: IO ()
main = do
  putStrLn "hello there!"
  args <- getArgs
  let inputFile  = args !! 0
  let outputFile = args !! 1
  putStrLn $ "input file: " ++ inputFile
  putStrLn $ "output file: " ++ outputFile
  inputText <- readFile inputFile
  putStrLn "=======File contents below========="
  putStrLn inputText
  putStrLn "======Displayed file contents======"
  let tokenDefinitions = filter (not . null) $ Split.splitOn "\n\n" inputText
  forM_ tokenDefinitions $ \tr -> do
    putStrLn "<tokenRule>"
    putStrLn tr
    putStrLn "</tokenRule>\n\n\n"
{-  let tokenRules = mergeUniqueRules $
                   map (S.unions .
                        map stringToRules .
                        Split.splitOn "\n")
                   tokenDefinitions-}
  let tokenRules = mergeUniqueRules $ map (S.unions .
                        map stringToRules .
                        Split.splitOn "\n")
                   tokenDefinitions
  putStrLn "<==========TOKEN RULES=================>"
  putStrLn $ show tokenRules
  putStrLn "adfasdfasdfadsfasdfasdf"
  forM_ tokenRules $ \tr -> do
    putStrLn "<TR>"
    putStrLn $ show tr
    putStrLn "</TR>\n\n"
  putStrLn "////////////////////////////////////////\n"
  let stateMap = mapFromTupleSet tokenRules
  putStrLn $ show stateMap
  let ndfa = makeNDFA stateMap
  putStrLn "NDFA below"
  putStrLn $ show ndfa

  let dfa  = determinize ndfa
  putStrLn "DFA below"
  putStrLn $ show dfa
