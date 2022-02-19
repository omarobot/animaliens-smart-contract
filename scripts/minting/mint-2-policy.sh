
user=${1:-buyer}

./scripts/minting/test-mint-tx.sh \
  $(cardano-cli-balance-fixer collateral --address $(cat ~/$BLOCKCHAIN_PREFIX/$user.addr) $BLOCKCHAIN) \
   scripts/test-policies/test-policy-1.plutus \
   $(cat scripts/test-policies/test-policy-1-id.txt) 746f6b656e31 1 $user
