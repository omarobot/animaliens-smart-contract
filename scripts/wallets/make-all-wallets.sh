set -eux
mkdir -p scripts/temp/
mkdir -p ~/$BLOCKCHAIN_PREFIX
./scripts/wallets/make-wallet-and-pkh.sh seller
./scripts/wallets/make-wallet-and-pkh.sh buyer
./scripts/wallets/make-wallet-and-pkh.sh team
./scripts/wallets/make-wallet-and-pkh.sh project
./scripts/wallets/make-wallet-and-pkh.sh community
