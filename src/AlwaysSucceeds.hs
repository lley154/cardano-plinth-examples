-- BEGIN pragmas
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- END pragmas

module AlwaysSucceeds where

-- BEGIN imports

import PlutusTx.Blueprint
import PlutusTx.Prelude
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import PlutusTx (CompiledCode, compile)

-- END imports
-- BEGIN MyRedeemer annotations

{-# ANN R0 (SchemaComment "Redeemer 0") #-}
{-# ANN R1 (SchemaComment "Redeemer 1") #-}
{-# ANN R2 (SchemaComment "Redeemer 2") #-}

-- END MyRedeemer annotations
-- BEGIN interface types

type MyDatum = Integer
data MyRedeemer = R0 | R1 V3.Lovelace | R2 V3.Value

-- END interface types
-- BEGIN makeIsDataSchemaIndexed

$(makeIsDataSchemaIndexed ''MyRedeemer [('R0, 0), ('R1, 1), ('R2, 2)])

-- END makeIsDataSchemaIndexed
-- BEGIN derived instances

deriving stock instance Generic MyRedeemer
deriving anyclass instance HasBlueprintDefinition MyRedeemer

-- END derived instances
-- BEGIN validator

typedValidator :: MyDatum -> MyRedeemer -> Bool
typedValidator _datum redeemer =
  case redeemer of
    R0   -> True
    R1{} -> True
    R2{} -> True

untypedValidator :: BuiltinData -> BuiltinUnit
untypedValidator scriptContext =
  check
    $ case V3.unsafeFromBuiltinData scriptContext of
      V3.ScriptContext
        _txInfo
        (V3.Redeemer redeemer)
        (V3.SpendingScript _ (Just (V3.Datum datum))) ->
          typedValidator
            (V3.unsafeFromBuiltinData datum)
            (V3.unsafeFromBuiltinData redeemer)
      _ -> False

{-# INLINEABLE untypedValidator #-}

alwaysSucceedsScript :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysSucceedsScript =
  $$(PlutusTx.compile [||untypedValidator||])

-- END validator