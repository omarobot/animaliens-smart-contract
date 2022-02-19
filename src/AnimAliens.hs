{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}

module AnimAliens where

import           Data.Map             as Map
import qualified Prelude              as Haskell
--
import           Control.Monad        hiding (fmap)
import qualified PlutusTx
import           PlutusTx.Prelude     as P
import           Ledger               hiding (singleton)
import           Ledger.Credential    (Credential (..))
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada hiding (divide)
import           Prelude              ((/), Float, toInteger, floor)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import           Cardano.Api hiding (Value, TxOut)
import           Cardano.Api.Shelley hiding (Value, TxOut)
import           Codec.Serialise hiding (encode)
import qualified Plutus.V1.Ledger.Api as Plutus
import GHC.Generics

-- Contract
-- Total Fee: 8%

data ContractInfo = ContractInfo
    { policySpaceBudz :: !CurrencySymbol
    , policyBid :: !CurrencySymbol
    , prefixSpaceBud :: !BuiltinByteString
    , prefixSpaceBudBid :: !BuiltinByteString
    , team :: !(PubKeyHash, Integer)
    , project :: !(PubKeyHash, Integer)
    , community :: !(PubKeyHash, Integer)
    , minPrice :: !Integer
    , bidStep :: !Integer
    } deriving (Generic, ToJSON, FromJSON)


-- Data and Redeemers

data TradeDetails = TradeDetails
    { tradeOwner :: !PubKeyHash
    , budId :: !BuiltinByteString
    , requestedAmount :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

instance Eq TradeDetails where
    {-# INLINABLE (==) #-}
    -- tradeOwner is not compared, since tradeOwner can changes with each trade/higher bid
    a == b = (budId  a == budId  b) &&
             (requestedAmount a == requestedAmount b)

data TradeDatum = StartBid | Bid TradeDetails | Offer TradeDetails
    deriving (Generic, ToJSON, FromJSON)

-- only compare necessary types in order to save storage!!
instance Eq TradeDatum where
    {-# INLINABLE (==) #-}
    StartBid == StartBid = True
    Bid a == Bid b = a == b


data TradeAction = Buy | Sell | BidHigher | Cancel
    deriving (Generic, ToJSON, FromJSON)

-- Validator

{-# INLINABLE tradeValidate #-}
tradeValidate :: ContractInfo -> TradeDatum -> TradeAction -> ScriptContext -> Bool
tradeValidate contractInfo@ContractInfo{..} tradeDatum tradeAction context = case tradeDatum of
    StartBid -> case tradeAction of
        BidHigher -> correctStartBidOutputs

    Bid details@TradeDetails{..} -> case tradeAction of
        BidHigher ->
            Bid details == scriptOutputDatum && -- expected correct script output datum
            Ada.fromValue (scriptInputValue) + Ada.lovelaceOf bidStep <= Ada.fromValue scriptOutputValue && -- expected correct bid amount
            containsPolicyBidNFT scriptOutputValue budId && -- expected correct bidPolicy NFT
            case txInfo `txSignedBy` tradeOwner of True -> True; False -> Ada.fromValue (valuePaidTo txInfo tradeOwner) >= Ada.fromValue scriptInputValue -- expected previous bidder refund
        Sell ->
            scriptOutputDatum == StartBid && -- expected correct script output datum
            containsPolicyBidNFT scriptOutputValue budId && -- expected correct bidPolicy NFT
            containsSpaceBudNFT (valuePaidTo txInfo tradeOwner) budId && -- expected bidder to be paid
            correctSplit (getLovelace (Ada.fromValue scriptInputValue)) signer -- expected ada to be split correctly
        Cancel ->
            txInfo `txSignedBy` tradeOwner && -- expected correct owner
            scriptOutputDatum == StartBid && -- expected correct script output datum
            containsPolicyBidNFT scriptOutputValue budId -- expected correct bidPolicy NFT

    Offer TradeDetails{..} -> case tradeAction of
        Buy ->
            traceIfFalse "missing nft" (containsSpaceBudNFT (valuePaidTo txInfo signer) budId) && -- expected buyer to be paid
            traceIfFalse "too low requested amount" (requestedAmount >= minPrice) && -- expected at least minPrice buy
            traceIfFalse "wrong split" (correctSplit requestedAmount tradeOwner) -- expected ada to be split correctly
        Cancel ->
            txInfo `txSignedBy` tradeOwner -- expected correct owner

    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo context

        policyAssets :: Value -> CurrencySymbol -> [(CurrencySymbol, TokenName, Integer)]
        policyAssets v cs = P.filter (\(cs',_,am) -> cs == cs' && am == 1) (flattenValue v)

        signer :: PubKeyHash
        signer = case txInfoSignatories txInfo of
            [pubKeyHash] -> pubKeyHash

        (teamPubKeyHash, teamFee1) = team
        (projectPubKeyHash, projectFee1) = project
        (communityPubKeyHash, communityFee1) = community

        -- minADA requirement forces the contract to give up certain fee recipients
        correctSplit :: Integer -> PubKeyHash -> Bool
        correctSplit lovelaceAmount tradeRecipient =
          let
            amount1 = lovelacePercentage lovelaceAmount (teamFee1)
            amount2 = lovelacePercentage lovelaceAmount (projectFee1)
            amount3 = lovelacePercentage lovelaceAmount (communityFee1)
          in
            Ada.fromValue (valuePaidTo txInfo teamPubKeyHash) >= Ada.lovelaceOf amount1 && -- expected team to receive right amount
            Ada.fromValue (valuePaidTo txInfo projectPubKeyHash) >= Ada.lovelaceOf amount2 && -- expected project to receive right amount
            Ada.fromValue (valuePaidTo txInfo communityPubKeyHash) >= Ada.lovelaceOf amount3 && -- expected community to receive right amount
            Ada.fromValue (valuePaidTo txInfo tradeRecipient) >= Ada.lovelaceOf (lovelaceAmount - amount1 - amount2 - amount3) -- expected trade recipient to receive right amount


        lovelacePercentage :: Integer -> Integer -> Integer
        lovelacePercentage am p = (am * 10) `divide` p


        outputInfo :: TxOut -> (Value, TradeDatum)
        outputInfo o = case txOutAddress o of
            Address (ScriptCredential _) _  -> case txOutDatumHash o of
                Just h -> case findDatum h txInfo of
                    Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                        Just b -> (txOutValue o, b)

        policyBidLength :: Value -> Integer
        policyBidLength v = length $ policyAssets v policyBid

        containsPolicyBidNFT :: Value -> BuiltinByteString -> Bool
        containsPolicyBidNFT v tn = valueOf v policyBid (TokenName (prefixSpaceBudBid <> tn)) >= 1

        containsSpaceBudNFT :: Value -> BuiltinByteString -> Bool
        containsSpaceBudNFT v tn = valueOf v policySpaceBudz (TokenName (prefixSpaceBud <> tn)) >= 1

        -- eager evaluation to check correct script inputs for all branches!!
        scriptInputValue :: Value
        !scriptInputValue =
            let
                isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
                    Nothing -> False
                    Just _  -> True
                xs = [ txOutValue (txInInfoResolved i)  | i <- txInfoInputs txInfo, isScriptInput i]
            in
                case xs of
                    [v] -> v -- normally just one script input is allowed
                    [v1, v2] ->
                        -- allow 2 script inputs if it's a combination of cancelling bid and buying OR cancelling offer and selling (same SpaceBud only)
                        case tradeDatum of
                            (Bid TradeDetails{..}) -> case (containsPolicyBidNFT v1 budId, containsSpaceBudNFT v2 budId) of
                                    (True, True) -> v1 -- expected script input 1 to contain bid token and script input 2 SpaceBud
                                    (False, False) -> v2 -- expected script input 2 to contain bid token and script input 1 SpaceBud
                            (Offer TradeDetails{..}) -> case (containsSpaceBudNFT v1 budId, containsPolicyBidNFT v2 budId) of
                                (True, True) -> v1 -- expected script input 2 to contain bid token and script input 1 SpaceBud
                                (False, False) -> v2 -- expected script input 1 to contain bid token and script input 2 SpaceBud


        scriptOutputValue :: Value
        scriptOutputDatum :: TradeDatum
        (scriptOutputValue, scriptOutputDatum) = case getContinuingOutputs context of
            [o] -> outputInfo o

        -- 2 outputs possible because of distribution of inital bid NFT tokens and only applies if datum is StartBid
        correctStartBidOutputs :: Bool
        correctStartBidOutputs = if policyBidLength scriptInputValue > 1
            then
                case getContinuingOutputs context of
                    [o1, o2] -> let (info1, info2) = (outputInfo o1, outputInfo o2) in
                                case info1 of
                                    (v1, StartBid) ->
                                        policyBidLength scriptInputValue - 1 == policyBidLength v1 && -- expected correct policyBid NFTs amount in output
                                        case info2 of
                                            (v2, Bid TradeDetails{..}) ->
                                                containsPolicyBidNFT v2 budId && -- expected policyBid NFT in output
                                                getLovelace (Ada.fromValue v2) >= minPrice && -- expected at least minPrice bid
                                                requestedAmount == 1 -- expeced correct output datum amount
                                    (v1, Bid TradeDetails{..}) ->
                                        containsPolicyBidNFT v1 budId && -- expected policyBid NFT in output
                                        getLovelace (Ada.fromValue v1) >= minPrice && -- expected at least minPrice bid
                                        requestedAmount == 1 && -- expeced correct output datum amount
                                        case info2 of
                                            (v2, StartBid) ->
                                                policyBidLength scriptInputValue - 1 == policyBidLength v2 -- expect correct policyBid NFTs amount in output
            else
                case getContinuingOutputs context of
                    [o] -> let (value, datum) = outputInfo o in case datum of
                            (Bid TradeDetails{..}) ->
                                containsPolicyBidNFT value budId && -- expected policyBid NFT in output
                                getLovelace (Ada.fromValue value) >= minPrice && -- expected at least minPrice bid
                                requestedAmount == 1 -- expeced correct output datum amount

data Trade
instance Scripts.ValidatorTypes Trade where
    type instance RedeemerType Trade = TradeAction
    type instance DatumType Trade = TradeDatum

tradeInstance :: ContractInfo -> Scripts.TypedValidator Trade
tradeInstance contractInfo = Scripts.mkTypedValidator @Trade
    ($$(PlutusTx.compile [|| tradeValidate ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TradeDatum @TradeAction

tradeValidator :: ContractInfo -> Validator
tradeValidator = Scripts.validatorScript . tradeInstance


tradeAddress :: ContractInfo -> Ledger.Address
tradeAddress = scriptAddress . tradeValidator

-- Types

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo , 0)]
PlutusTx.makeLift ''ContractInfo

PlutusTx.makeIsDataIndexed ''TradeDetails [ ('TradeDetails, 0)]
PlutusTx.makeLift ''TradeDetails

PlutusTx.makeIsDataIndexed ''TradeDatum [ ('StartBid, 0)
                                        , ('Bid,   1)
                                        , ('Offer, 2)
                                        ]
PlutusTx.makeLift ''TradeDatum

PlutusTx.makeIsDataIndexed ''TradeAction [ ('Buy,       0)
                                         , ('Sell,      1)
                                         , ('BidHigher, 2)
                                         , ('Cancel,    3)
                                        ]
PlutusTx.makeLift ''TradeAction



-- helper functions

lovelacePercentage :: Integer -> Integer -> Integer
lovelacePercentage am p = (am * 10) `Haskell.div` p

-- Serialization

{-
    As a Script
-}

tradeScript :: ContractInfo -> Plutus.Script
tradeScript = Plutus.unValidatorScript . tradeValidator

{-
    As a Short Byte String
-}

tradeSBS :: ContractInfo -> SBS.ShortByteString
tradeSBS = SBS.toShort . LBS.toStrict . serialise . tradeScript

{-
    As a Serialised Script
-}

tradeSerialised :: ContractInfo -> PlutusScript PlutusScriptV1
tradeSerialised = PlutusScriptSerialised . tradeSBS

newContractInfo :: CurrencySymbol -> BuiltinByteString -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ContractInfo
newContractInfo pid tokenPrefix teamPkh projectPkh communityPkh = ContractInfo
    { policySpaceBudz = pid
    , policyBid = "800df05a0cc6b6f0d28aaa1812135bd9eebfbf5e8e80fd47da9989eb"
    , prefixSpaceBud = tokenPrefix
    , prefixSpaceBudBid = "SpaceBudBid"
    , team = (teamPkh, 500) -- 2.0%
    , project = (projectPkh, 500) -- 2.0%
    , community = (communityPkh, 250) -- 4.0%
    , minPrice = 70000000
    , bidStep = 10000
    }
