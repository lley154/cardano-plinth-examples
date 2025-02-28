# Cardano Plinth (PlutusTx) Examples
## Setup
Install docker desktop for either Mac, Windows or Linux https://docs.docker.com/desktop/

Run the docker desktop appication to confirm that docker engine is able to start
![image](https://github.com/user-attachments/assets/54aa5ab4-aa1f-453a-9a60-3d07a0b7a711)

In a terminal window on the host machine
```
$ git clone https://github.com/lley154/cardano-plinth-examples
$ cd cardano-plinth-examples
$ docker run \
  -v /paht-to-woring-directory/cardano-plinth-examples:/workspaces/cardano-plinth-examples  \
  -it ghcr.io/input-output-hk/devx-devcontainer:x86_64-linux.ghc96-iog
```
## Compiling Haskell Files
Inside the container run the following commands
```
[workspaces] cd cardano-plinth-examples
[workspaces/cardano-plinth-examples] cabal update
[workspaces/cardano-plinth-examples] cabal build
```
## Generating Blueprints
The cabal build will take a while and once it is completed, you should be able to execute the following commands to generate the blueprint files.
```
[workspaces/cardano-plinth-examples] cabal run faucet-validator-blueprint -- ./off-chain/faucet-validator-blueprint.json
```

## Running a local Cardano devnet
In new a terminal window, download the yaci-devkit Github repo
```
$ git clone https://github.com/bloxbean/yaci-devkit.git
$ cd yaci-devkit
$ git checkout v0.10.0-preview5
```

Now run the following commands to start up the yaci devkit
```
$ ./bin/devkit.sh start
yaci-cli:>create-node -o --era conway
devnet:default>start
```
This will start up a local cardano node and devnet.  Wait for the ```Yaci Store Started``` to appear on the terminal


You can find out the API and URLs by using the ```info``` command
```
devnet:default>info

###### Node Details (Container) ######
[💡 Node port] 3001
[💡 Node Socket Paths] 
/clusters/nodes/default/node/node.sock
[💡 Submit Api Port] 8090
[💡 Protocol Magic] 42
[💡 Block Time] 1.0 sec
[💡 Slot Length] 1.0 sec
[💡 Start Time] 1738176728
[💡 Epoch Length] 600
[💡 Security Param] 300
[💡 SlotsPerKESPeriod] 129600


#################### URLS (Host) ####################
[💡 Yaci Viewer] http://localhost:5173
[💡 Yaci Store Swagger UI] http://localhost:8080/swagger-ui.html
[💡 Yaci Store Api URL] http://localhost:8080/api/v1/
[💡 Pool Id] pool1wvqhvyrgwch4jq9aa84hc8q4kzvyq2z3xr6mpafkqmx9wce39zy


#################### Other URLS ####################
[💡 Ogmios Url (Optional)] ws://localhost:1337
[💡 Kupo Url   (Optional)] http://localhost:1442


#################### Node Ports ####################
[💡 n2n port] localhost:3001
[💡 n2c port (socat)] localhost:3333
```

## Test the contracts
Launch a new terminal window on your host machine
```
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
```
Close and then reopen a new terminal window
```
$ nvm install v22.13.0
$ cd cardano-plinth-examples/test
$ npm install
```

Notes: There will be some deprecated and security warnings that you can ignore for now since this only used for testing purposes.

Now run the faucet end-to-end integration test cases using the yaci devkit local devnet.
```
$ npm run test-faucet
```

The tests should run successfully and you should see the following output
```
 PASS  test/faucet.test.ts (30.199 s)
  E2E Faucet Test
    ✓ Mint access token (3510 ms)
    ✓ Mint faucet tokens (3104 ms)
    ✓ Lock faucet tokens (3182 ms)
    ✓ Withdrawal Faucet Token (3175 ms)
    ✓ Withdrawal Faucet Token (3070 ms)
    ✓ Check Wallet Balance (18 ms)

Test Suites: 1 passed, 1 total
Tests:       6 passed, 6 total
Snapshots:   0 total
Time:        30.607 s
Ran all test suites.
```

In the terminal window runnign yaci devkit, you can query the utxos at and address using the following command
```
devnet:default>utxos addr_test1qz3y5kacdctuxzczarxu2t3c9jswawm4xtp3n26t6l9qelyh40hlltg24lyeuw9mk3e6p7fs58sv852zmp5suuk85s9qmzzvlm
1. 4ce6cf1d49c1108c980151e0ab793ecedab297a90aeeba478e61eb3a131e0c47#1 : [Amount(unit=lovelace, quantity=2000000), Amount(unit=b10b3a5a819392a156d0190ba4a5c34f1706bad47c8ed404dfe193e16163636573732d746f6b656e, quantity=1), Amount(unit=bb07253073ca06fb0b2704c41ad26a869ec303dd12f39d4409639a366661756365742d746f6b656e, quantity=100)]
--------------------------------------------------------------------------------------
2. 4ce6cf1d49c1108c980151e0ab793ecedab297a90aeeba478e61eb3a131e0c47#2 : [Amount(unit=lovelace, quantity=5000000)]
```

You can also go to the Yaci viewer http://localhost:5173/ and view the transactions as well.

![image](https://github.com/user-attachments/assets/871ee952-9945-4d79-9ad3-ad569252a911)

![image](https://github.com/user-attachments/assets/9c6c96e5-ae4b-4a92-8a81-54c97d47387f)





