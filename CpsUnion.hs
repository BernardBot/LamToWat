{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module CpsUnion where

import Val
import OpenUnion
 
data Base a
  = ADD' Val Val String a
  | APP' Val [Val]
  | DONE' Val
  deriving (Show, Functor)

data Record a
  = RECORD' [Val] String a
  | SELECT' Int Val String a
  deriving (Show, Functor)

data Fun a
  = FIX' [(String, [String], a)] a
  deriving (Show, Functor)

data Malloc a
  = MALLOC' Int String a
  | LOAD' Int Val String a
  | STORE' Int Val Val a
  deriving (Show, Functor)

done v          = inject (DONE' v)
app v vs        = inject (APP' v vs)
add v1 v2 x e   = inject (ADD' v1 v2 x e)
record vs x e   = inject (RECORD' vs x e)
select i v x e  = inject (SELECT' i v x e)
fix fs e        = inject (FIX' fs e)
malloc i x e    = inject (MALLOC' i x e)
load i v x e    = inject (LOAD' i v x e)
store i v1 v2 e = inject (STORE' i v1 v2 e)

pattern DONE v          <- (project -> Just (DONE' v))
pattern APP v vs        <- (project -> Just (APP' v vs))
pattern ADD v1 v2 x e   <- (project -> Just (ADD' v1 v2 x e))
pattern RECORD vs x e   <- (project -> Just (RECORD' vs x e))
pattern SELECT i v x e  <- (project -> Just (SELECT' i v x e))
pattern FIX fs e        <- (project -> Just (FIX' fs e))
pattern MALLOC i x e    <- (project -> Just (MALLOC' i x e))
pattern LOAD i v x e    <- (project -> Just (LOAD' i v x e))
pattern STORE i v1 v2 e <- (project -> Just (STORE' i v1 v2 e))
