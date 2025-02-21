{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           AlwaysSucceeds
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import           PlutusTx.Blueprint
import           System.Environment          (getArgs)

-- BEGIN contract blueprint declaration

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "my-contract"
    , contractPreamble = myPreamble -- defined below
    , contractValidators = Set.singleton myValidator -- defined below
    , contractDefinitions = deriveDefinitions @[MyDatum, MyRedeemer]
    }

-- END contract blueprint declaration
-- BEGIN preamble declaration

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "My Contract"
    , preambleDescription = Just "A simple contract"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV3
    , preambleLicense = Just "MIT"
    }

-- END preamble declaration
-- BEGIN validator blueprint declaration
myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "AlwaysSucceeds Validator"
    , validatorDescription = Just "An example validator"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "My Redeemer"
          , argumentDescription = Just "A redeemer that does something awesome"
          , argumentPurpose = Set.fromList [Spend, Mint]
          , argumentSchema = definitionRef @MyRedeemer
          }
    , validatorDatum =
        Just
          MkArgumentBlueprint
            { argumentTitle = Just "My Datum"
            , argumentDescription = Just "A datum that contains something awesome"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @MyDatum
            }
    , validatorCompiled = do
        let script = alwaysSucceedsScript
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV3 code)
    }

-- END validator blueprint declaration

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args) 