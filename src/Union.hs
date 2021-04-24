{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Union where

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

deriving instance (Show (sigl n b p r q), Show (sigr n b p r q)) => Show ((sigl :+: sigr) n b p r q)

instance (ShowSig a, ShowSig b) => ShowSig (a :+: b) where
  showSig (L a) = "L (" ++ showSig a ++ ")"
  showSig (R a) = "R (" ++ showSig a ++ ")"

instance (PPrintable (sigl n b p r q), PPrintable (sigr n b p r q)) => PPrintable ((sigl :+: sigr) n b p r q) where
  pprint (L a) = "L (" ++ pprint a ++ ")"
  pprint (R a) = "R (" ++ pprint a ++ ")"

instance (PPrintableSig sigl, PPrintableSig sigr) => PPrintableSig (sigl :+: sigr) where
  pprintSig (L a) = "L (" ++ pprintSig a ++ ")"
  pprintSig (R a) = "R (" ++ pprintSig a ++ ")"
  


