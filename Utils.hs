module Utils where

import qualified Data.Set as Set

a ∪ b = a `Set.union` b
a ∩ b = a `Set.intersection` b
a ∈ b = a `Set.member` b
a ⊂ b = a `Set.member` b
