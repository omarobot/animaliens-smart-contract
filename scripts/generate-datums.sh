set -eu
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

prefix=${1:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix

sellerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/seller-pkh.txt)
buyerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/buyer-pkh.txt)

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/buy.json
{
  "constructor": 2,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$sellerPkh"
        },
        {
          "bytes": "31"
        },
        {
          "int": 70000000
        }
      ]
    }
  ]
}

EOF
