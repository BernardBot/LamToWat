{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tree.Union where

import Tree.Syntax

data (:+:) :: Sig -> Sig -> Sig where
  L :: sigl n b p r q -> (sigl :+: sigr) n b p r q
  R :: sigr n b p r q -> (sigl :+: sigr) n b p r q
infixr 7 :+:

class (sub :: Sig) :<: (sup :: Sig) where
  inj :: sub n b p r q -> sup n b p r q

instance f :<: f where
  inj = id

instance {-# OVERLAPPING #-} f :<: (f :+: g) where
  inj = L

instance f :<: g => f :<: (h :+: g) where
  inj = R . inj
