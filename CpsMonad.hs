module CpsMonad where

import Val
import Control.Monad

data Cps a
  = DONE a
  | APP Val [Val]
  | ADD Val Val (Val -> Cps a)
  | RECORD [Val] (Val -> Cps a)
  | SELECT Int Val (Val -> Cps a)
  | FIX [(String,[String],Cps Val)] (Cps a)
  | FRESH String (String -> Cps a)
  | GETK (Val -> Cps a)
  | SETK Val (Cps a)
  | BLOCK (Cps Val) (Val -> Cps a)

instance Monad Cps where
  DONE a       >>= f = f a
  APP v vs     >>= f = APP v vs
  ADD v1 v2 k  >>= f = ADD v1 v2 (\ x -> k x >>= f)
  RECORD vs k  >>= f = RECORD vs (\ x -> k x >>= f)
  SELECT i v k >>= f = SELECT i v (\ x -> k x >>= f)
  FIX fs k     >>= f = FIX fs (k >>= f)
  FRESH x k    >>= f = FRESH x (\ x -> k x >>= f)
  GETK k       >>= f = GETK (\ x -> k x >>= f)
  SETK v k     >>= f = SETK v (k >>= f)
  BLOCK b k    >>= f = BLOCK b (\ x -> k x >>= f)

instance Applicative Cps where
  pure = DONE
  (<*>) = ap

instance Functor Cps where
  fmap = liftM

done       = DONE
app        = APP
add v1 v2  = ADD v1 v2 DONE
record vs  = RECORD vs DONE
select i v = SELECT i v DONE
fix fs     = FIX fs (DONE ())
fresh x    = FRESH x DONE
getk       = GETK DONE
setk v     = SETK v (DONE ())
block b    = BLOCK b DONE
