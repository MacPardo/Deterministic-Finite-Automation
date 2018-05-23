import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Control.Applicative as Ap
import System.Environment (getArgs)
import Control.Monad (forM_)

data Symbol = TerminalSymbol Literal
            | NonTerminalSymbol State
            deriving (Show, Eq, Ord)

data State = State String
           | Error
           deriving (Show, Eq, Ord)

data Literal = Literal Char
             | Epsilon
             deriving (Show, Eq, Ord)

type Rule = (State, [Symbol])
type DFA  = M.Map State (M.Map Literal State) --Deterministic Finite Automation
type Grammar  = M.Map State (S.Set [Symbol])

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
stringToSymbol (x:[]) = TerminalSymbol $
                        Literal x
stringToSymbol (a:ys@(b:c:xs))
  | a == '<' && last ys == '>' = NonTerminalSymbol $
                                 State $
                                 init ys
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
                          (NonTerminalSymbol a) -> a
        rulesBodies = Split.splitOn " | " ruleBody
        ruleBody = splitArray !! 1
        ruleHead = splitArray !! 0
        splitArray = Split.splitOn " ::= " s

tokenStringToRules :: String -> S.Set Rule
tokenStringToRules s = S.fromList $ Ap.getZipList $
                       (,) <$> z rules <*> z bodies
  where bodies = (Ap.getZipList $ (\a b -> [a,b]) <$>
                   z (init symbols) <*>
                   z (map NonTerminalSymbol $ tail rules))
                 ++ [[last symbols]]
        symbols = map stringToSymbol $ separateSymbols s
        rules = map (State . ('_':) . show) [1..]
        z = Ap.ZipList

mergeUniqueRules :: [S.Set Rule] -> S.Set Rule
mergeUniqueRules rs = S.unions $ gz $ f <$> z prefixes <*> z rs
  where f p ruleSet = S.map g ruleSet
          where prefixBody (NonTerminalSymbol (State stateName)) = NonTerminalSymbol $ State $ p ++ stateName
                prefixBody symbol = symbol
                g (State s, a) = (State $ p ++ s, map prefixBody a)
        prefixes = map ((++"_") . show) [1..]
        z = Ap.ZipList
        gz = Ap.getZipList

--makeGrammar :: S.Set Rule -> Grammar
makeGrammar ruleSet = S.foldr' f M.empty ruleSet
  where f (state, symbols) acc = M.insert state newSymbols acc
          where newSymbols = case ms of
                               Nothing -> S.singleton symbols
                               Just s  -> s `S.union` (S.singleton symbols)
                ms = M.lookup state acc

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
  --let tokenRules = map Split.splitOn "\n" tokenDefinitions
  let tokenRules = mergeUniqueRules $
                   map (S.unions .
                        map stringToRules .
                        Split.splitOn "\n")
                   tokenDefinitions
  putStrLn "<==========TOKEN RULES=================>"
  putStrLn $ show tokenRules
  putStrLn "////////////////////////////////////////\n"
  let fullGrammar = makeGrammar tokenRules
  putStrLn "<===========FULL GRAMMAR===============>"
  putStrLn $ show fullGrammar
  putStrLn "////////////////////////////////////////"
  return ()
