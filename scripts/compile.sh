set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..
tempDir=$mainDir/temp

policyId=${1:-d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2}
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
