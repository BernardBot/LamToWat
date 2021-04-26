{-# LANGUAGE FlexibleContexts #-}
module Cont.Cps2Cps where

import Control.Monad.Writer
import Control.Monad.Reader

import Data.Tuple

import Val
import Types

import Cont.Cps

type TransM = WriterT [Fun Cps] (Reader [Var])

cps2cps :: Cps -> Cps
cps2cps =
  uncurry FIX .
  swap .
  flip runReader [] .
  runWriterT .
  c2c

c2c :: Cps -> TransM Cps
c2c (FIX fs e) = do
  fs' <- mapM funClos fs
  tell fs'
  c2c e
  where funClos (name,args,body) = do
          nv <- ask
          body' <- local (++args) (c2c body)
          return $
            ( name
            , "_closure" : args
            , SELECT 1 (VAR "_closure") "_env" $
              foldr (openClos args) body' (zip [0..] nv)
            )

        openClos args (i,x) =
          if x `elem` args then id else SELECT i (VAR "_env") x

c2c (APP fun args) = do
  nv <- ask
  return $
    RECORD (map VAR nv) "_env" $
    foldr mkClos appClos (fun:args)
  where appClos = case fun of
          LABEL fp -> let cl = '_' : fp in
                        APP (LABEL fp) (VAR cl : args')
          VAR cl   -> let fp = '_' : cl in
                        SELECT 0 (VAR cl) fp $
                        APP (VAR fp) (VAR cl : args')

        mkClos (LABEL x) = RECORD [LABEL x, VAR "_env"] ('_' : x)
        mkClos _ = id

        args' = map rename args

        rename (LABEL x) = VAR $ '_' : x
        rename v = v

c2c (RECORD vs x e) = withvar x e $ RECORD vs
c2c (SELECT i v x e) = withvar x e $ SELECT i v
c2c (ADD v1 v2 x e) = withvar x e $ ADD v1 v2
c2c (DONE v) = return $ DONE v

withvar :: Var -> Cps -> (Var -> Cps -> Cps) -> TransM Cps
withvar x e op = do
  e' <- local (++[x]) (c2c e)
  return $ op x e'
