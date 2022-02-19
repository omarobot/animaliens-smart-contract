The "Buy Now" contract for animaliens. This ia a modified Spacebudz with a percent of the purchases going to the team, project and community wallets.

## Creating the Script Address

After compiling the smart contract, it is necessary to make a script address.

First source either the testnet or mainnet environment variables.

For testnet

```
$ source scripts/envars/testnet-env.envars
```

For mainnet

```
$ source scripts/envars/mainnet-env.envars
```

The environment variable files set `CARDANO_NODE_SOCKET_PATH` to the path of the appropriate Daedalus socket file (either Testnet Daedalus or the regular mainnet Daedalus). It you run a `cardano-node` on your own you should set this environment variable to your socket file location after sourcing the environment variable file.

First create the wallets and get the protocol parameters.

```
$ ./scripts/wallets/make-all-wallets.sh
$ ./scripts/query-protocol-parameters.sh
```

Next, run:

```bash
scripts/compile.sh
```

This will make the files `scripts/testnet/buy.addr` or `scripts/mainnet/buy.addr`.

## Example Transactions

The folder `scripts/core` has parameterized example transactions. This are used by the wrappers in `scripts/happy-path` and `scripts/failure-cases`. The various transactions are combined in test scripts in the folder `scripts/tests`.

## Running the Tests

To run the tests run `scripts/tests/all.sh`
