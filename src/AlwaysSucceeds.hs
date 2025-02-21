{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

module AlwaysSucceeds where

import PlutusTx.Prelude (BuiltinData, BuiltinUnit, check, ($))
import PlutusLedgerApi.Data.V3 qualified as V3Data
import PlutusTx (CompiledCode, compile)

untypedValidator :: BuiltinData -> BuiltinUnit
untypedValidator scriptContext =
  check
    $ V3Data.unsafeFromBuiltinData scriptContext

{-# INLINEABLE untypedValidator #-}
alwaysSucceedsScript :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysSucceedsScript =
  $$(compile [||untypedValidator||])