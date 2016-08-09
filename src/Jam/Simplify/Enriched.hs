module Jam.Simplify.Enriched
  (
    toSimple
  ) where

import qualified Jam.Language.Lambda.Enriched as Enriched
import qualified Jam.Language.Lambda.Simple   as Simple

toSimple :: Enriched.Expr -> Simple.Expr
toSimple = undefined
