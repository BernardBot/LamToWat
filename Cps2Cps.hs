module Cps2Cps where

import Cps hiding (M)

import Control.Monad.Writer
import Control.Monad.Reader

import Data.Tuple
import Data.List
import Data.Maybe

-- REWRITE

-- def f(x):
--   x = 10
--   def g(y);
--     [y,x,x] -- add at the end?
--     -- also do not overwrite y...
--     select y y
--     select x x 
--     select x x
--     return x + y
--   [x,x]
--   g(1)

-- cps2cps = hoist . closify
-- -- function to variables defined at declaration map
-- dv (RECORD _ x e)   xs = dv e (x:xs)
-- dv (SELECT _ _ x e) xs = dv e (x:xs)
-- dv (ADD _ _ x e)    xs = dv e (x:xs)
-- dv (DONE _)         _  = []
-- dv (APP _ _)        _  = []
-- dv (FIX fs e)       xs = dv e xs ++
--   concatMap (\ (f,as,b) -> (f,nub xs\\as) : dv b (nub (as++xs))) fs

-- closify e = go e
--   where go (RECORD vs x e)  = RECORD vs x (go e) -- traverse the Cexp AST
--         go (SELECT i v x e) = SELECT i v x (go e)
--         go (ADD v1 v2 x e)  = ADD v1 v2 x (go e)
--         -- actually do some transforming
--         go (DONE v)         = close [v] $ DONE (renameC v)
--         go (APP v vs)       = close (v:vs) $ (apply v) (map renameC vs)
--         go (FIX fs e)       = FIX (map goFunc fs) (go e)

--         -- closify function of 'FIX'
--         -- add closure variable + open closure in function
--         goFunc (f,as,b) = (f,envName:as,open (lookdvs f) (go b))

--         -- free variable function map
--         dvs = dv e []
--         -- lookup free variable of function named 'f'
--         lookdvs f = fromJust $ lookup f dvs

--         -- rename function values to closure values
--         renameC (LABEL f) = VAR (f ++ closTag)
--         renameC v = v -- we do nothing for non-function values

--         -- the strings we will be using for renaming
--         closTag = "_C"
--         funcTag = "_F"
--         envName = ".env"

--         -- create closures from a list of values and a Cexp 'k' to do later
--         close [] k = k
--         close (LABEL f:vs) k = 
--           RECORD (LABEL f : map VAR (lookdvs f)) (f ++ closTag) (close vs k)
--         close (_:vs) k = close vs k -- do nothing for non-function values

--         -- select all fields (except the first, which is a function pointer) from
--         -- the closure record
--         open xs k = foldr (\ (i,x) -> SELECT i (VAR envName) x) k (zip [1..] xs)

--         -- apply a closure
--         -- for function labels we need to rename where we select from
--         -- the closure created in the 'close' and 'go' function
--         -- the names need to match, thus we use the 'closTag' as suffix
--         apply (LABEL f) vs = APP (LABEL f) (VAR (f ++ closTag):vs)
--         apply (VAR f) vs = SELECT 0 (VAR f) f_F (APP (VAR f_F) (VAR f:vs))
--           where f_F = f ++ funcTag
--         -- number application is an error

-- -- hoist all function definitions to top level
-- -- we now get one big fix as program (= letrec)
-- hoist (APP v vs)       = FIX [] (APP v vs)
-- hoist (DONE v)         = FIX [] (DONE v)               
-- hoist (RECORD rs x e)  = case hoist e of FIX fs e -> FIX fs (RECORD rs x e)
-- hoist (SELECT i v x e) = case hoist e of FIX fs e -> FIX fs (SELECT i v x e)
-- hoist (ADD v1 v2 x e)  = case hoist e of FIX fs e -> FIX fs (ADD v1 v2 x e)
-- hoist (FIX fs e)       = case hoist e of FIX fs e -> FIX (fs'++fs) e
--   where fs' = concatMap (\ (f,as,b) -> case hoist b of FIX fs e -> (f,as,e) : fs) fs

cps2cps :: Cps -> Cps
cps2cps =
  uncurry FIX .
  swap .
  flip runReader [] .
  runWriterT .
  c2c

type M =  WriterT [Fun] (Reader [String])

-- bugged, do it the original way...
c2c :: Cps -> M Cps
c2c (FIX fs e) = do
  nv <- ask
  fs' <- mapM (\ (f,as,b) -> do
        b' <- local (++as) (c2c b)
        return (f,_nv:as,open b' as nv)) fs
  tell fs'
  c2c e
c2c (APP v vs) = do
  nv <- ask
  let vs' = rename vs
  let app = apply v vs'
  return (close app (v:vs) nv)
c2c (DONE v) = return (DONE v)
c2c (RECORD vs x e)  = local (++[x]) (c2c e >>= return . RECORD vs x)
c2c (SELECT i v x e) = local (++[x]) (c2c e >>= return . SELECT i v x)
c2c (ADD v1 v2 x e)  = local (++[x]) (c2c e >>= return . ADD v1 v2 x)

open :: Cps -> [String] -> [String] -> Cps
open e as nv = SELECT 1 (VAR _nv) _nv
  (foldr (\ (i,x) b -> if x `elem` as then b else SELECT i (VAR _nv) x b) e (zip [0..] nv))

apply :: Val -> [Val] -> Cps
apply (LABEL f) vs = APP (LABEL f) (VAR (_p f) : vs)
apply (VAR f) vs = SELECT 0 (VAR f) (_p f) (APP (VAR (_p f)) (VAR f:vs))

close :: Cps -> [Val] -> [String] -> Cps
close e vs nv = RECORD (map VAR nv) _nv
  (foldr (\ v b -> case v of LABEL f -> RECORD [v,VAR _nv] (_p f) b; _ -> b) e vs)

rename :: [Val] -> [Val]
rename vs = map (\ v -> case v of LABEL f -> VAR (_p f); _ -> v) vs

_p = ('_' :)
_nv = "_nv"
