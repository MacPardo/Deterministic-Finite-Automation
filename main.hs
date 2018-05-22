import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Control.Applicative as Ap

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

isRule :: String -> Bool
isRule = (" ::= " `List.isInfixOf`)

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
tokenStringToRules s = S.fromList $ Ap.getZipList $ (,) <$>
                       z rules <*>
                       z ((Ap.getZipList $ (\a b -> [a,b]) <$>
                                   z (init symbols) <*>
                                   z (map NonTerminalSymbol $ tail rules))
                           ++ [[last symbols]])
  where symbols = map stringToSymbol $ separateSymbols s
        rules = map (State . ('_':) . show) [1..]
        z = Ap.ZipList
