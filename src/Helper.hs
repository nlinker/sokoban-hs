module Helper (str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Multiline strings without explicit support from the language
-- example:
-- ```
-- message = [str|
--    Multiline
--    message
--    goes
--    here
-- |]
-- ```
str :: QuasiQuoter
str = QuasiQuoter
  { quoteDec = error "multiline strings are not allowed to be in declarations"
  , quotePat = error "multiline strings are not allowed to be in patterns"
  , quoteType = error "multiline strings are not allowed to be in type signatures"
  , quoteExp = stringE
  }

