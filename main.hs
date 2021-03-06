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
                    deriving (Eq, Ord)
type NonTerminalSymbol = String
type Symbol = Either TerminalSymbol NonTerminalSymbol
type Rule = (NonTerminalSymbol, [Symbol])

data State = State (String, Int, Bool)
           | InitialState Int
           | UInitialState -- universal initial state
           | FinalState Int
           | ErrorState
           deriving (Eq, Ord)

instance Show TerminalSymbol where
  show (TerminalSymbol c) = [c]
  show Epsilon = [epsilonRepresentation]

instance Show State where
  show ErrorState = "ERROR"
  show (FinalState n) = "'*" ++ (show n) ++ "'"
  show (InitialState n) = "'#" ++ (show n) ++ "'"
  show UInitialState = "$"
  show (State (s, n, b)) = "'" ++ f ++ "(" ++ s ++ "," ++ (show n) ++ ")" ++ "'"
    where f | b         = "*"
            | otherwise = ""

type NDFA = M.Map State (M.Map TerminalSymbol (S.Set State))
type DFA =  M.Map (S.Set State) (M.Map TerminalSymbol (S.Set State))
{-
No DFA um conjunto de estados é tratado como se fosse um estado só
-}

setJson :: S.Set String -> String
setJson s = "[" ++ (List.intercalate "," . S.toList $ s) ++ "]"

mapJson :: M.Map String String -> String
mapJson m = "{" ++
  (List.intercalate "," . map (\(k, v) -> k ++ ":" ++ v) $ M.assocs m)
  ++ "}"

quotes :: String -> String
quotes s = "\"" ++ s ++ "\""

ndfaJson :: NDFA -> String
ndfaJson a = mapJson . M.map g . M.mapKeys (quotes . show) $ a
  where g v = mapJson . M.map h . M.mapKeys (quotes . show) $ v
        h s = quotes . setJson . S.map show $ s

dfaJson :: DFA -> String
dfaJson a = mapJson . M.map g . M.mapKeys (quotes . f) $ a
  where f k = setJson . S.map show $ k
        g v = mapJson . M.map h . M.mapKeys (quotes . show) $ v
        h s = quotes . setJson . S.map show $ s

  
epsilonRepresentation = '&'

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
stringToSymbol (x:[])
  | x == epsilonRepresentation = Left $ Epsilon
  | otherwise = Left $ TerminalSymbol x
stringToSymbol (a:ys@(b:c:xs))
  | a == '<' && last ys == '>' = Right $ init ys
stringToSymbol _ = undefined

stringIsRule :: String -> Bool
stringIsRule = (" ::= " `List.isInfixOf`)

{-
transforma a linha de uma gramatica em
um conjunto de regras
-}
grammarLineToRules :: String -> S.Set Rule
grammarLineToRules s = S.fromList $
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
 
{-
recebe um token, gera a sua gramatica e
retorna o conjunto de regras
-}
tokenStringToRules :: String -> S.Set Rule
tokenStringToRules s = S.fromList $ Ap.getZipList $
                       (,) <$> z rules <*> z bodies
  where bodies = (Ap.getZipList $ (\a b -> [a, Right b]) <$>
                   z (init symbols) <*>
                   z (tail rules))
                 ++ [[last symbols]]
        symbols = map stringToSymbol $ separateSymbols s
        rules = ["$"] ++ (map show [2..])
        z = Ap.ZipList

 
{-
verfica se a entrada eh um token ou a linha de uma gramatica,
e chama a grammarLineToRules ou tokenStringToRules
-}
stringToRules :: String -> S.Set Rule
stringToRules s
  | stringIsRule s = grammarLineToRules s
  | otherwise = tokenStringToRules s

isInitial :: String -> Bool
isInitial = ("$" `List.isInfixOf`)
  
mergeUniqueRules :: [S.Set Rule] -> S.Set (State, [Either TerminalSymbol State])
mergeUniqueRules rs = S.unions $ gz $ f <$> z [1..] <*> z rs
  where f p ruleSet = S.map g ruleSet
          where prefixBody (Right nts)
                  | isInitial nts = Right $ (InitialState p)
                  | otherwise = Right $ State (nts, p, False)
                prefixBody (Left ts) = Left ts
                g (ts, a) = (if isInitial ts
                             then InitialState p
                             else State (ts, p, False),
                             map prefixBody a)
        z = Ap.ZipList
        gz = Ap.getZipList

mapFromTupleSet :: (Ord t, Ord k) => S.Set (t, k) -> M.Map t (S.Set k)
mapFromTupleSet ruleSet = S.foldr' f M.empty ruleSet
  where f (state, symbols) acc = M.insert state newSymbols acc
          where newSymbols = case ms of
                               Nothing -> S.singleton symbols
                               Just s  -> s `S.union` (S.singleton symbols)
                ms = M.lookup state acc

makeSingleInitialState :: NDFA -> NDFA
makeSingleInitialState a = M.map (M.map $ S.map ini) . M.mapKeys ini $ a


ini (InitialState _) = UInitialState
ini a = a

initialRuleBodies :: M.Map State [(TerminalSymbol, State)] -> [(TerminalSymbol, State)]
initialRuleBodies = concat . M.elems . M.filterWithKey (\k v -> isIni k)
  where isIni (InitialState _) = True
        isIni _ = False

makeNDFA :: M.Map State (S.Set [Either TerminalSymbol State]) -> NDFA
makeNDFA m = M.map (mapFromTupleSet . S.fromList) .
             M.filterWithKey (\k v -> not . isIni $ k) .
             (\m -> M.insert UInitialState (initialRuleBodies m) m) .
             M.map (map aux) .
             M.mapWithKey ((\k l -> map (f k) l)) .
             M.map S.toList $
             m
  where f (State (s, n, qf)) (Left a:[]) = (a      , FinalState n)
        f (InitialState n)   (Left a:[]) = (a      , FinalState n)
        f key (Left a:Right b:[])        = (a      , b)
        f key (Right b:[])               = (Epsilon, b)
        f _   _ = error "invalid rule in makeNDFA"
        aux (a, InitialState _) = (a, UInitialState)
        aux a = a
        isIni (InitialState _) = True
        isIni _ = False

dfaTerminals :: DFA -> S.Set TerminalSymbol
dfaTerminals =  S.fromList . concat . M.elems . M.map (M.keys)

dfaStates :: DFA -> S.Set State
dfaStates = S.unions . M.keys

addErrorState :: DFA -> DFA
addErrorState a = M.map h a
  where terminals = dfaTerminals a
        h m = M.fromList . S.toList $ S.map f terminals
          where f v
                  | isJust $ M.lookup v m = (v, fromJust $ M.lookup v m)
                  | otherwise = (v, S.fromList [ErrorState])

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
  
determinize :: NDFA -> DFA
determinize a = let stateSets = (concat $ map M.elems $ M.elems a) ++ (ndfaInitialStatesSingletons a)
                    test = M.fromList $  map (\set -> (set, mapOfSetsUnions . S.toList . f $ set)) stateSets
  in test

  where f set = S.map fromJust $ S.filter isJust $ S.map aux set
        aux state = M.lookup state a

missingStates :: DFA -> S.Set (S.Set State)
missingStates a = transitionStates `S.difference` lineStates
  where lineStates = S.fromList $ M.keys a
        transitionStates = S.fromList . concat . map (M.elems) . M.elems $ a

haveStatesInCommon :: S.Set State -> S.Set State -> Bool
haveStatesInCommon a b = not . S.null . S.intersection a $ b

getMissingStateMap :: DFA -> S.Set State -> M.Map TerminalSymbol (S.Set State)
getMissingStateMap a state = M.fromList . map f $ terms
  where terms = S.toList . dfaTerminals $ a
        commonStates = filter (haveStatesInCommon state) . M.keys $ a
        f t = (t, commonTransitions)
          where commonTransitions = S.unions . map fromJust . filter isJust . map trans $ commonStates
                trans s = M.lookup s a >>=
                          \m -> M.lookup t m


getMissingStateMap' a state = commonStates --M.fromList . map f $ terms
  where terms = S.toList . dfaTerminals $ a
        commonStates = filter (haveStatesInCommon state) . M.keys $ a
        f t = (t, commonTransitions)
          where commonTransitions = S.unions . map fromJust . filter isJust . map trans $ commonStates
                trans s = M.lookup s a >>= (M.lookup t)

  
addMissingStates :: DFA -> DFA
addMissingStates a
  | null missing = a
  | otherwise = addMissingStates newDfa
  where missing = S.toList . missingStates $ a
        missingDfa = M.fromList . map (\s -> (s, getMissingStateMap a s)) $ missing
        newDfa = missingDfa `M.union` a 


ndfaInitialStatesSingletons :: NDFA -> [S.Set State]
ndfaInitialStatesSingletons = map S.singleton . filter f . M.keys
  where f (InitialState _) = True
        f UInitialState = True
        f _ = False

initialStates :: DFA -> S.Set (S.Set State)
initialStates = S.fromList . filter dfaIsStateInitial . M.keys

dfaIsStateInitial :: S.Set State -> Bool
dfaIsStateInitial = any f . S.toList
  where f (InitialState _) = True
        f _ = False

dfaIsStateFinal :: S.Set State -> Bool
dfaIsStateFinal = any f . S.toList
  where f (FinalState _) = True
        f _ = False
{-
remove todos os estados que não pertencem ao feixo transitivo
direto dos estados iniciais
-}
removeUnreachables :: DFA -> DFA
removeUnreachables a = M.filterWithKey f a
  where reachable = reachableStates a
        f k v = k `S.member` reachable

--reachableStates :: DFA -> S.Set (S.Set State)
--reachableStates a = S.fromList $
--                       concat $
--                       map (S.toList . dfaStateClojure a S.empty)
--                           (S.toList $ initialStates a)

reachableStates :: DFA -> S.Set (S.Set State)
reachableStates a = dfaStateClojure a S.empty (S.singleton UInitialState)


{-retorna o feixo transitivo de um estado-}
dfaStateClojure :: DFA -> S.Set (S.Set State) -> S.Set State -> S.Set (S.Set State)
dfaStateClojure a ok s = case directClojure >>= (\clj -> Just $ S.foldr f ok' clj) of
                           (Just a) -> a
                           Nothing  -> S.empty
  where ok' = s `S.insert` ok
        directClojure = M.lookup s a >>= (\m -> Just $ (S.fromList . M.elems $ m) `S.difference` ok')
        f state acc = acc `S.union` (dfaStateClojure a acc state)

isStateProductive :: DFA -> S.Set State -> Bool
isStateProductive a state = any dfaIsStateFinal . S.toList $ clj
  where clj = dfaStateClojure a S.empty state


removeUnproductives :: DFA -> DFA
removeUnproductives a = M.map (M.filter $ isStateProductive a) . M.filterWithKey isKeyProductive $ a
  where isKeyProductive k v = isStateProductive a k

main :: IO ()
main = do
  args <- getArgs
  let inputFile  = args !! 0
  putStrLn $ "input file: " ++ inputFile
  inputText <- readFile inputFile
  let tokenDefinitions = filter (not . null) $ Split.splitOn "\n\n" inputText
  let tokenRules = mergeUniqueRules $ map (S.unions .
                        map stringToRules .
                        Split.splitOn "\n")
                   tokenDefinitions
  let stateMap = mapFromTupleSet tokenRules
  let ndfa = makeNDFA stateMap
  putStrLn "NDFA below"
  putStrLn $ ndfaJson ndfa

  let dfa  = addMissingStates . determinize $ ndfa
  putStrLn "\nDFA below"
  putStrLn $ dfaJson dfa

  putStrLn "\nDFA without unreachable states"
  putStrLn $ dfaJson $ removeUnreachables $ dfa

  putStrLn "\nDFA without unproductive states"
  putStrLn $ dfaJson $ removeUnproductives $ dfa

  putStrLn "\nDFA without unproductive and unreachable states"
  putStrLn $ dfaJson $ removeUnproductives $ removeUnreachables $ dfa

  putStrLn "\nDFA with error states"
  putStrLn $ dfaJson $ removeUnproductives $ removeUnreachables $ dfa

