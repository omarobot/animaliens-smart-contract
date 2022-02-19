set -eux

thisDir=$(dirname "$0")

$thisDir/lock-cancel.sh
$thisDir/lock-swap.sh
$thisDir/nft-goes-to-wrong-address.sh
$thisDir/too-little-ada.sh
$thisDir/too-little-fees.sh
