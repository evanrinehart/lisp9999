module Lisp where

import Prelude hiding (print)
import Text.Parsec
import Data.List
import System.IO.Unsafe

data S =
  Y String |
  N Integer |
  S [S]
    deriving (Show)

print :: S -> String
print (N n) = show n
print (Y y) = y
print (S ss) = "(" ++ intercalate " " (map print ss) ++ ")"

lread :: String -> S
lread src = ans where
  ans = case runParser s () "source" src of
    Left err -> error (show err)
    Right s -> s
  s = choice [num, sym, sexpr]
  num = fmap (N . read) (many1 digit)
  sym = fmap Y $ many1 (noneOf " \n\t()")
  sexpr = do
    string "("
    spaces
    ss <- sepBy s spaces
    spaces
    string ")"
    spaces
    return (S ss)

repl :: IO ()
repl = do
  getLine >>= putStrLn . print . eval . lread
  repl
    

-- special forms list
-- (list e e e e)
-- (let e (x e) (y e))
-- (lambda (x y z) e)
-- (cond (p e) (p e) (p e))
-- (+ e e)
-- (- e e)
-- (< e e)
-- (= e e)
-- (car e)
-- (cdr e)
-- (cons e e)
-- (syscall 13 e)

eval :: S -> S
eval (Y y) = Y y
eval (N n) = N n
eval (S []) = S []
eval (S ((Y "let"):e:pairs)) = ans where
  bindings = letargs pairs
  ans = eval $ foldr rewrite e bindings
eval (S [Y "let"]) = error "let requires at least two args"
eval (S [Y "lambda",params,e]) = S [Y "lambda",params,e]
eval (S ((Y "list"):es)) = S es
eval (S [Y "cond"]) = error "cond requires at least one condition"
eval (S ((Y "cond"):(S [p,e]):ps)) = case eval p of
  Y "true" -> eval e
  Y "false" -> eval (S ((Y "cond"):ps))
  _ -> error "cond conditions must evaluate to true or false"
eval (S [Y "+", e1, e2]) = add (eval e1) (eval e2)
eval (S [Y "<", e1, e2]) = lt (eval e1) (eval e2)
eval (S [Y "=", e1, e2]) = eq (eval e1) (eval e2)
eval (S [Y "car", e]) = car (eval e)
eval (S [Y "cdr", e]) = cdr (eval e)
eval (S [Y "cons", e1, e2]) = cons (eval e1) (eval e2)
eval (S [Y "syscall", e1, e2]) = syscall (eval e1) (eval e2)
eval (S (f:args)) = eval $ apply (eval f) (map eval args)

add :: S -> S -> S
add (N a) (N b) = N (a+b)
add _ _ = error "+ takes two numbers"

lt :: S -> S -> S
lt (N a) (N b) = if a < b then Y "true" else Y "false"
lt _ _ = error "< takes two numbers"

eq :: S -> S -> S
eq x y = if x `equals` y then Y "true" else Y "false"

equals :: S -> S -> Bool
equals (N a) (N b) = a == b
equals (Y a) (Y b) = a == b
equals (S a) (S b) = all id (zipWith equals a b)
equals _ _ = False

apply :: S -> [S] -> S
apply (S [Y "lambda", S params, e]) args = ans where
  bindings = lambdaArgs params args
  ans = foldr rewrite e bindings
apply _ _ = error "apply only works with well-formed lambdas"

-- rewrite occurrences of symbol with expression
-- unless you encounter a let or lambda which uses the same variable
rewrite :: (String,S) -> S -> S
rewrite (v,arg) (Y x) | v == x = arg
                      | v /= x = Y x
rewrite (v,arg) (N i) = N i
rewrite (v,arg) (S ((Y "let"):e:pairs)) = case filterBindings (map fst $ letargs pairs) [(v,arg)] of
  [] -> S $ (Y "let"):e:pairs
  [binding] -> S $ (Y "let"):(rewrite binding e):(rewritePairs binding pairs)
rewrite (v,arg) (S [Y "lambda",params,e]) = case filterBindings (lambdaParams params) [(v,arg)] of
  [] -> S [Y "lambda",params,e]
  [binding] -> S [Y "lambda",params,rewrite binding e]
rewrite (v,arg) (S ((Y "list"):es)) = S $ (Y "list"):(map (rewrite (v,arg)) es)
rewrite (v,arg) (S ((Y "cond"):pairs)) = S $ (Y "cond"):(rewritePairs (v,arg) pairs)
rewrite (v,arg) (S xs) = S (map (rewrite (v,arg)) xs)

rewritePairs (v,arg) pairs = map f pairs where
  f (S [e1, e2]) = S [rewrite (v,arg) e1, rewrite (v,arg) e2]
  f _ = error "invalid pair encountered"

letargs :: [S] -> [(String, S)]
letargs pairs = map ident pairs where
  ident (S [Y y, e]) = (y, e)
  ident _ = error "invalid let"

filterBindings :: [String] -> [(String,S)] -> [(String,S)]
filterBindings shadows set = filter (not . shadowed) set where
  shadowed (v,e) = v `elem` shadows

lambdaParams :: S -> [String]
lambdaParams (S ss) = map f ss where
  f (Y y) = y
  f _ = error "invalid lambda params"

lambdaArgs :: [S] -> [S] -> [(String,S)]
lambdaArgs params args = zipWith mush params args where
  mush (Y v) e = (v,e)
  mush _ _ = error "invalid lambda params"

car :: S -> S
car (S (s:ss)) = s
car (S []) = error "car used on an empty list"
car _ = error "car only works on lists"

cdr :: S -> S
cdr (S (s:ss)) = S ss
cdr (S []) = error "used cdr on an empty list"
cdr _ = error "cdr only works on lists"

cons :: S -> S -> S
cons x (S ss) = S (x:ss)
cons x _ = error "cons only works on lists"

syscall :: S -> S -> S
syscall (N n) arg = case n of
  13 -> unsafePerformIO (putStrLn "*** SYSTEM CALL 13 ***" >> return (Y "ok"))
  _ -> error ("there is no syscall "++show n)

