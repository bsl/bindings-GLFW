module Bindings.Helpers where

import Language.Haskell.TH (DecsQ, reportWarning)

warn :: String -> DecsQ
warn str = reportWarning str >> return []
