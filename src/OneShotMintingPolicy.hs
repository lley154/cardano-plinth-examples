{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

module OneShotMintingPolicy where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion110)

import PlutusLedgerApi.V1.Value(geq, leq) 
import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.V3.Contexts qualified as V3Contexts
import PlutusLedgerApi.Data.V3 qualified as V3Data
import PlutusTx (CompiledCode, compile, liftCode, makeLift, makeIsDataSchemaIndexed, unsafeApplyCode)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (any, Bool, Bool(..), BuiltinData, BuiltinUnit, check, Maybe(Just, Nothing), mempty, traceIfFalse, (==), ($), (&&))

import PlutusTx.Blueprint

data OneShotMintingParams = OneShotMintingParams { utxoRef :: V3.TxOutRef} 
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

makeLift ''OneShotMintingParams
makeIsDataSchemaIndexed ''OneShotMintingParams [('OneShotMintingParams, 0)]

data OneShotMintingRedeemer = MintToken V3.TokenName | BurnToken V3.TokenName
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OneShotMintingRedeemer [('MintToken, 0), ('BurnToken, 1)]

{-# INLINEABLE oneShotTypedMintingPolicy #-}
oneShotTypedMintingPolicy :: 
    OneShotMintingParams -> 
    OneShotMintingRedeemer -> 
    V3.ScriptContext -> 
    Bool
oneShotTypedMintingPolicy params redeemer ctx =
    case redeemer of    
        MintToken tn -> traceIfFalse "UTXO not found" hasUTxO &&
                        traceIfFalse "Invalid minted amount" (checkMintedAmount tn)

        BurnToken tn -> traceIfFalse "Invalid burned amount" (checkBurnedAmount tn)
    where
        info :: V3.TxInfo
        info = V3.scriptContextTxInfo ctx

        ownSymbol :: V3.CurrencySymbol
        ownSymbol = V3Contexts.ownCurrencySymbol ctx

        minted :: V3.Value
        minted = V3.mintValueMinted $ V3Contexts.txInfoMint info

        hasUTxO :: Bool
        hasUTxO = any (\i -> V3.txInInfoOutRef i == utxoRef params) $ V3.txInfoInputs info

        checkMintedAmount :: V3.TokenName -> Bool
        checkMintedAmount tokenName = geq (currencyValueOf minted ownSymbol) (V3.singleton ownSymbol tokenName 1)

        checkBurnedAmount :: V3.TokenName -> Bool
        checkBurnedAmount tokenName = leq (currencyValueOf minted ownSymbol) (V3.singleton ownSymbol tokenName (-1))


{-# INLINABLE currencyValueOf #-}
currencyValueOf :: V3.Value -> V3.CurrencySymbol -> V3.Value
currencyValueOf (V3.Value m) c = case Map.lookup c m of
    Nothing -> mempty
    Just t  -> V3.Value (Map.singleton c t)


oneShotUntypedMintingPolicy ::
  OneShotMintingParams ->
  BuiltinData ->
  BuiltinUnit
oneShotUntypedMintingPolicy params ctx =
  check
    $ case V3Data.unsafeFromBuiltinData ctx of
      V3Data.ScriptContext
        _txInfo
        (V3Data.Redeemer redeemer)
        _spendingScript ->
          oneShotTypedMintingPolicy
            params (V3.unsafeFromBuiltinData redeemer) (V3.unsafeFromBuiltinData ctx)


oneShotMintingPolicyScript ::
  OneShotMintingParams ->
  CompiledCode (BuiltinData -> BuiltinUnit)
oneShotMintingPolicyScript params =
  $$(compile [||oneShotUntypedMintingPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 params
