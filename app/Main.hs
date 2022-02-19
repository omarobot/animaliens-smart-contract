{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Ledger hiding (singleton)
import Options.Applicative

import AnimAliens
import Data.String
import Plutus.V1.Ledger.Value(TokenName(..))

data Opts = Opts
  { team :: PubKeyHash
  , project :: PubKeyHash
  , community :: PubKeyHash
  , policyId :: CurrencySymbol
  , tokenNamePrefix :: String
  , output :: FilePath
  } deriving Show

main :: IO ()
main = createSC =<< execParser opts

opts :: ParserInfo Opts
opts = info (optsParser <**> helper) . mconcat $
  [ fullDesc
  , progDesc "Create a smart contract"
  ]

optsParser :: Parser Opts
optsParser = Opts
  <$> (strOption . mconcat $
    [ long "team"
    , metavar "PKH"
    ])
  <*> (strOption . mconcat $
    [ long "project"
    , metavar "PKH"
    ])
  <*> (strOption . mconcat $
    [ long "community"
    , metavar "PKH"
    ])
  <*> (strOption . mconcat $
    [ long "policy-id"
    , metavar "PID"
    ])
  <*> (strOption . mconcat $
    [ long "token-name-prefix"
    , metavar "PREFIX"
    ])
  <*> (strOption . mconcat $
    [ long "output"
    , metavar "FILE"
    , help "Where to write the script."
    ])

createSC :: Opts -> IO ()
createSC Opts{..} = do

  -- let tokenNameBytes = unTokenName $ fromString tokenNamePrefix
  let tokenNameBytes = fromString tokenNamePrefix

  result <- writeFileTextEnvelope output Nothing . tradeSerialised $ newContractInfo policyId tokenNameBytes team project community
  case result of
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote validator to file " ++ output
