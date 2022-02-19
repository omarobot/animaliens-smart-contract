cardano-cli address build \
  --payment-script-file ./scripts/buy.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/buy.addr
