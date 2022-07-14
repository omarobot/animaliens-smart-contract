set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..
tempDir=$mainDir/temp

policyId=${1:-1ff89104c2c3826b21ea8a8471e383c26f31257f3b2d7889b8fe1763}
teamPkh=${2:-$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/team-pkh.txt)}
projectPkh=${3:-$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/project-pkh.txt)}
communityPkh=${4:-$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/community-pkh.txt)}

(
cd $mainDir
cabal run exe:create-smart-contract -- \
  --output=scripts/buy.plutus \
  --policy-id=$policyId \
  --team=$teamPkh \
  --project=$projectPkh \
  --community=$communityPkh \
  --token-name-prefix=token
)

$thisDir/hash-plutus.sh
