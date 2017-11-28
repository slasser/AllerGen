-- To do : make sure functions are sharing necessary stack information with one another

import AllStarLib
import qualified Data.Set as DS

parseS [] = error ("out of tokens while parsing S")
parseS tokens@(first : rest) =
  let expansion = case adaptivePredict (NT "S") tokens [] atnEnv of
                    Just 0  -> [eat (T "If"), parseE, eat (T "Then"), parseS, eat (T "Else"), parseS]
                    Just 1  -> [eat (T "Begin"), parseS, parseL]
                    Just 2  -> [eat (T "Print"), parseE]
                    Nothing -> error "No viable alt while parsing S"
      (subtrees, tokens') = parseSubtrees tokens expansion
  in  (Node "S" subtrees, tokens')
  
parseL [] = error ("out of tokens while parsing " ++ "L")
parseL tokens@(first : rest) =
  let expansion = case adaptivePredict (NT "L") tokens [] atnEnv of
                    Just 0  -> [eat (T "End")]
                    Just 1  -> [eat (T ";"), parseS, parseL]
                    Nothing -> error "No viable alt while parsing L"
      (subtrees, tokens') = parseSubtrees tokens expansion
  in  (Node "L" subtrees, tokens')
  
parseE [] = error ("out of tokens while parsing " ++ "E")
parseE tokens@(first : rest) =
  let expansion = case adaptivePredict (NT "E") tokens [] atnEnv of
                    Just 0  -> [eat (T "Num"), eat (T "=="), eat (T "Num")]
                    Nothing -> error "No viable alt while parsing E"
      (subtrees, tokens') = parseSubtrees tokens expansion
  in  (Node "E" subtrees, tokens')

atnEnv :: AtnEnv
atnEnv = DS.fromList [-- First path through the S ATN
                      (Q_Init "S", GramSym Eps, Q_Middle "S" 0 0),
                      (Q_Middle "S" 0 0, GramSym (T "If"), Q_Middle "S" 0 1),
                      (Q_Middle "S" 0 1, GramSym (NT "E"), Q_Middle "S" 0 2),
                      (Q_Middle "S" 0 2, GramSym (T "Then"), Q_Middle "S" 0 3),
                      (Q_Middle "S" 0 3, GramSym (NT "S"), Q_Middle "S" 0 4),
                      (Q_Middle "S" 0 4, GramSym (T "Else"), Q_Middle "S" 0 5),
                      (Q_Middle "S" 0 5, GramSym (NT "S"), Q_Middle "S" 0 6),
                      (Q_Middle "S" 0 6, GramSym Eps, Q_Final "S"),
                      -- Second path through the S ATN
                      (Q_Init "S", GramSym Eps, Q_Middle "S" 1 0),
                      (Q_Middle "S" 1 0, GramSym (T "Begin"), Q_Middle "S" 1 1),
                      (Q_Middle "S" 1 1, GramSym (NT "S"), Q_Middle "S" 1 2),
                      (Q_Middle "S" 1 2, GramSym (NT "L"), Q_Middle "S" 1 3),
                      (Q_Middle "S" 1 3, GramSym Eps, Q_Final "S"),
                      -- Third path through the S ATN
                      (Q_Init "S", GramSym Eps, Q_Middle "S" 2 0),
                      (Q_Middle "S" 2 0, GramSym (T "Print"), Q_Middle "S" 2 1),
                      (Q_Middle "S" 2 1, GramSym (NT "E"), Q_Middle "S" 2 2),
                      (Q_Middle "S" 2 2, GramSym Eps, Q_Final "S"),
                      -- First path through the L ATN
                      (Q_Init "L", GramSym Eps, Q_Middle "L" 0 0),
                      (Q_Middle "L" 0 0, GramSym (T "End"), Q_Middle "L" 0 1),
                      (Q_Middle "L" 0 1, GramSym Eps, Q_Final "L"),
                      -- Second path through the L ATN
                      (Q_Init "L", GramSym Eps, Q_Middle "L" 1 0),
                      (Q_Middle "L" 1 0, GramSym (T ";"), Q_Middle "L" 1 1),
                      (Q_Middle "L" 1 1, GramSym (NT "S"), Q_Middle "L" 1 2),
                      (Q_Middle "L" 1 2, GramSym (NT "L"), Q_Middle "L" 1 3),
                      (Q_Middle "L" 1 3, GramSym Eps, Q_Final "L"),
                      -- First (and only) path through the E ATN
                      (Q_Init "E", GramSym Eps, Q_Middle "E" 0 0),
                      (Q_Middle "E" 0 0, GramSym (T "Num"), Q_Middle "E" 0 1),
                      (Q_Middle "E" 0 1, GramSym (T "=="), Q_Middle "E" 0 2),
                      (Q_Middle "E" 0 2, GramSym (T "Num"), Q_Middle "E" 0 3),
                      (Q_Middle "E" 0 3, GramSym Eps, Q_Final "E")]


input1 = words "If Num == Num Then Print Num == Num Else Print Num == Num"
