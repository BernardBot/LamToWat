{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Tps.Union where

import Tps.Syntax

data (:+:) :: Sig -> Sig -> Sig where
  L :: sigl n b -> (sigl :+: sigr) n b
  R :: sigr n b -> (sigl :+: sigr) n b
infixr 7 :+:

infixr 6 :<:
class (:<:) (sub :: Sig) (sup :: Sig) where
  inj :: sub n b -> sup n b

instance a :<: a where
  inj = id

instance a :<: a :+: b where
  inj = L

instance {-# OVERLAPPABLE #-} a :<: c => a :<: b :+: c where
  inj = R . inj

