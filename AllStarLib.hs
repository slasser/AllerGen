module AllStarLib where

import Data.List
import qualified Data.Set as DS

-- Types (mostly copied from the GallStar definitions

data Symbol = T String | NT String | Eps deriving (Eq, Ord, Show)

data AtnState = Q_Init String
              | Q_Middle String Int Int
              | Q_Final String
              deriving (Eq, Ord, Show)

data AtnEdgeLabel = GramSym Symbol | Pred Bool deriving (Eq, Ord, Show)

type AtnEdge = (AtnState, AtnEdgeLabel, AtnState)

type AtnEnv = DS.Set AtnEdge

type AtnStack = [AtnState]

type AtnConfig = (AtnState, Int, AtnStack)

data DfaState = D_Init [AtnConfig]
              | D_Middle [AtnConfig]
              | D_Final Int
              | D_Error

type DfaEdge = (DfaState, String, DfaState)

type DfaEnv = [(Symbol, [DfaEdge])]

data Ast = Leaf String | Node String [Ast] deriving (Eq, Show) 

-- parser functions

move :: DfaState -> String -> AtnEnv -> [AtnConfig]
move d token atnEnv =
  let neighborConfigs (q, i, gamma) =
        let edges = outgoingEdges q atnEnv
            validEdges =
              filter (\edge -> case edge of
                                 (_, GramSym (T tName), _) -> token == tName
                                 (_, _, _) -> False)
                     edges
            neighbors = map (\(_, _, q') -> (q', i, gamma)) validEdges
        in  neighbors
  in  case d of
        D_Init configs -> concat $ map neighborConfigs configs
        D_Middle configs -> concat $ map neighborConfigs configs
        D_Final _ -> error "implementation error in move"
        D_Error   -> error "implementation error in move"

closure :: [AtnConfig] -> AtnEnv -> AtnConfig -> [AtnConfig]
closure busy atnEnv config@(q, i, gamma) =
  if elem config busy then
    []
  else
    let busy' = config : busy
        edges = outgoingEdges q atnEnv
        followEdge (_, GramSym (NT ntName), q') = closure busy' atnEnv (Q_Init ntName, i, q' : gamma)
        followEdge (_, GramSym Eps, q')         = closure busy' atnEnv (q', i, gamma)
        followEdge (_, Pred _, q')              = closure busy' atnEnv (q', i, gamma)
        followEdge (_, GramSym (T _), _)        = [config]
    in  case (q, gamma) of
          (Q_Final _, nil)         -> [config]
          (Q_Final _, q' : gamma') -> config : closure busy' atnEnv (q', i, gamma')
          (_, _)                   -> concat (map followEdge edges)

target :: DfaState -> String -> AtnEnv -> DfaState
target d token atnEnv =
  let neighborConfigs = move d token atnEnv
      reachableConfigs = concat $ map (closure [] atnEnv) neighborConfigs
  in  if null reachableConfigs then
        D_Error
      else
        let pathLabels = map (\(_, i, _) -> i) reachableConfigs
        in  case nub pathLabels of
              [] -> D_Error
              [i] -> D_Final i
              i : i2 : i's -> D_Middle reachableConfigs

startState :: Symbol -> AtnStack -> AtnEnv -> DfaState
startState (NT ntName) stack atnEnv =
  let edges = outgoingEdges (Q_Init ntName) atnEnv
      followEdge (Q_Init _, GramSym Eps, q'@(Q_Middle _ i _)) =
        closure [] atnEnv (q', i, stack)
      followEdge (Q_Init _, Pred b, q'@(Q_Middle _ i _)) =
        if b then closure [] atnEnv (q', i, stack) else []
      followEdge _ = error "malformed grammar"
      reachableConfigs = concat $ map followEdge edges
  in  D_Init reachableConfigs
startState _ _ _ = error "grammar symbol passed to startState must be NT"

sllPredict sym tokens0 d0 stack atnEnv =
  let predictionLoop d tokens =
        let advance d token tokens =
              let d' = target d token atnEnv
              in  predictionLoop d' tokens
            predictWithCurrConfigs configs =
              let finalConfigs = filter (\(Q_Final s, _, _) ->
                                            case sym of
                                              NT s2 -> s == s2
                                              _     -> False)
                                        configs
                  pathLabels = map (\(_, i, _) -> i) finalConfigs
              in  case nub pathLabels of
                    [i]          -> Just i
                    []           -> Nothing
                    i : i2 : i's -> Nothing
        in  case (d, tokens) of
              (D_Error, _)             -> Nothing
              (D_Final i, _)           -> Just i
              (D_Init _, tok : toks)  -> advance d tok toks
              (D_Middle _, tok : toks) -> advance d tok toks
              (D_Init configs, [])    -> predictWithCurrConfigs configs
              (D_Middle configs, [])   -> predictWithCurrConfigs configs
  in  predictionLoop d0 tokens0

adaptivePredict :: Symbol -> [String] -> AtnStack -> AtnEnv -> Maybe Int
adaptivePredict sym tokens stack atnEnv =
  let d0 = startState sym stack atnEnv
  in  sllPredict sym tokens d0 stack atnEnv


-- utility functions

outgoingEdge :: AtnState -> AtnEnv -> AtnEdge
outgoingEdge p atnEnv = let edges = outgoingEdges p atnEnv
                        in  case edges of
                              [edge] -> edge
                              _ -> error "Multiple edges found"

outgoingEdges :: AtnState -> AtnEnv -> [AtnEdge]
outgoingEdges p atnEnv = DS.toList (DS.filter (\(p',_,_) -> p' == p) atnEnv)

eat :: Symbol -> [String] -> (Ast, [String])
eat (NT _) _    = error "expected a terminal symbol"
eat (T _) []    = error "no tokens to eat"
eat (T tName) (t : ts) = if tName == t then (Leaf tName, ts) else error "token mismatch"

parseSubtrees :: [String] -> [([String] -> (Ast, [String]))] -> ([Ast], [String])
parseSubtrees tokens parseFuncs =
  foldl (\(subtrees, toks) f ->
            let (subtree, toks') = f toks
            in  (subtrees ++ [subtree], toks'))
        ([], tokens)
        parseFuncs
