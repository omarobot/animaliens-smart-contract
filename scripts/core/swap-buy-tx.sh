set -eux

baseDir="scripts"

bodyFile=temp/swap-tx-body.01
outFile=temp/swap-tx.01
buyerAddr=$1
sellerAddr=$2
teamAddr=$3
signingKey=$4
value=$5
sellerAmount=$6
teamAmount=$7
datumFile="${8}"
projectAddr="${9}"
projectAmount="${10}"
communityAddr="${11}"
communityAmount="${12}"
redeemerFile="${13}"
datumHash="${14}"
spenderAddress="${15}"
nftValidatorFile=$baseDir/buy.plutus
scriptHash=$(cat scripts/$BLOCKCHAIN_PREFIX/buy.addr)

utxoScript=$(scripts/query/sc.sh | grep $datumHash | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
changeOutput=$(cardano-cli-balance-fixer change --address $spenderAddress $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $spenderAddress $BLOCKCHAIN ) \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $spenderAddress $BLOCKCHAIN ) \
    --tx-out "$sellerAddr + $sellerAmount" \
    --tx-out "$buyerAddr + $value" \
    --tx-out "$teamAddr + $teamAmount" \
    --tx-out "$projectAddr + $projectAmount" \
    --tx-out "$communityAddr + $communityAmount" \
    --tx-out "$spenderAddress + 3000000 lovelace $extraOutput" \
    --change-address $spenderAddress \
    --protocol-params-file $baseDir/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --out-file $bodyFile

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile
