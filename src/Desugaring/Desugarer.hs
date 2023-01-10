module Desugaring.Desugarer
    ( desugar
    ) where

import           Common
import           Desugaring.Core
import           Errors
import           Parsing.Parser  (ExprExt)

desugar :: ExprExt -> ExprC
desugar _ = NilC
