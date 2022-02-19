set -eux

DATUM_PREFIX=${1:-0}

thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

$baseDir/core/swap-buy-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/team.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  "1724100 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.746f6b656e31" \
  "64400000 lovelace" \
  "1400000 lovelace" \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy.json \
  $(cat ~/$BLOCKCHAIN_PREFIX/project.addr) \
  "1400000 lovelace" \
  $(cat ~/$BLOCKCHAIN_PREFIX/community.addr) \
  "2800000 lovelace" \
  "$baseDir/shared-redeemers/buy-redeemer.json" \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy-hash.txt) \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
