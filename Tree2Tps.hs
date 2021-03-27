{-# LANGUAGE GADTs #-}

module Tree2Tps where

import Val
import Vec
import Option

import Tree
import Tps hiding (Node,Leaf)

import Control.Monad.State
import Control.Monad.Reader
-- need state monad with int and environment -> see cpscmdcompiler


