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
  uncurry Fix .
  swap .
  flip runReader [] .
  runWriterT .
  c2c

c2c :: Cps -> TransM Cps
c2c (Fix fs e) = do
  fs' <- mapM funClos fs
  tell fs'
  c2c e
  where funClos (name,args,body) = do
          nv <- ask
          body' <- local (++args) (c2c body)
          return $
            ( name
            , "_closure" : args
            , Select 1 (VAR "_closure") "_env" $
              foldr (openClos args) body' (zip [0..] nv)
            )

        openClos args (i,x) =
          if x `elem` args then id else Select i (VAR "_env") x

c2c (App fun args) = do
  nv <- ask
  return $
    Record (map VAR nv) "_env" $
    foldr mkClos appClos args
  where appClos = case fun of
          LABEL fp -> let cl = '_' : fp in
                        Record [LABEL fp,VAR "_env"] cl $
                        App (LABEL fp) (VAR cl : args')

          VAR cl   -> let fp = '_' : cl in
                        Select 0 (VAR cl) fp $
                        App (VAR fp) (VAR cl : args')

        mkClos (LABEL x) = Record [LABEL x, VAR "_env"] ('_' : x)
        mkClos _ = id

        args' = map rename args

        rename (LABEL x) = VAR $ '_' : x
        rename v = v

c2c (Record vs x e) = withvar x e $ Record vs
c2c (Select i v x e) = withvar x e $ Select i v
c2c (Add v1 v2 x e) = withvar x e $ Add v1 v2
c2c (Val v) = return $ Val v

withvar :: Var -> Cps -> (Var -> Cps -> Cps) -> TransM Cps
withvar x e op = do
  e' <- local (++[x]) (c2c e)
  return $ op x e'
