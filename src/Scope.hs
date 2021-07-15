{-# LANGUAGE RecordWildCards #-}

module Scope where

import Prelude hiding (lookup, insert)
import qualified Data.Map as M

data Scope k a
  = EmptyScope
  | Scope { parent :: Scope k a, valMap :: M.Map k a }

lookup :: Ord k => k -> Scope k a -> Maybe a
lookup key EmptyScope = Nothing
lookup key (Scope parent valMap) = case M.lookup key valMap of
  Nothing -> lookup key parent
  Just x -> Just x

insert :: Ord k => k -> a -> Scope k a -> Scope k a
insert key value EmptyScope = Scope EmptyScope $ M.fromList [(key, value)]
insert key value Scope { valMap = m, .. }
  = Scope { valMap = M.insert key value m, .. }

subScope :: Scope k a -> Scope k a
subScope scope = Scope scope M.empty

dropScope :: Scope k a -> Scope k a
dropScope EmptyScope = EmptyScope
dropScope Scope { parent = p, .. } = p


insertNew :: Ord k => k -> a -> Scope k a -> Maybe (Scope k a)

insertNew key value scope@Scope { valMap = m, .. } = case lookup key scope of
  Nothing -> Just $ insert key value scope
  Just _ -> Nothing

insertNew key value scope = insertNew key value (subScope scope)


