{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CTree.Union where

import Interpreter
import Types

data (:+:) :: Sig -> Sig -> Sig where
  L :: sigl n b p r q -> (sigl :+: sigr) n b p r q
  R :: sigr n b p r q -> (sigl :+: sigr) n b p r q
infixr 7 :+:

class (sub :: Sig) :<: (sup :: Sig) where
  inj :: sub n b p r q -> sup n b p r q
infixr 6 :<:

instance f :<: f where
  inj = id

instance {-# OVERLAPPING #-} f :<: (f :+: g) where
  inj = L

instance f :<: g => f :<: (h :+: g) where
  inj = R . inj

deriving instance (Show (sigl n b p r q),
                   Show (sigr n b p r q)) =>
                  Show ((sigl :+: sigr) n b p r q)

instance (Interpretable (sigl n b p r q),
          Interpretable (sigr n b p r q)) =>
         Interpretable ((sigl :+: sigr) n b p r q) where
  interp (L l) = interp l
  interp (R r) = interp r
