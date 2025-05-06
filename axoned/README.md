
# README

## Table of Contents

- [README](#readme)
  - [Table of Contents](#table-of-contents)
  - [Set Env](#set-env)
  - [Set Up Issuer Wallet](#set-up-issuer-wallet)
    - [Create Issuer Wallet](#create-issuer-wallet)
    - [Get AXONE Tokens](#get-axone-tokens)
    - [Check Issuer Wallet Balance](#check-issuer-wallet-balance)
    - [Get Account Number and Sequence](#get-account-number-and-sequence)
    - [Verify Generated DIDs](#verify-generated-dids)
  - [Set Up Smart Contracts](#set-up-smart-contracts)
    - [Objectarium](#objectarium)
    - [Dataverse](#dataverse)
  - [Create Hearth Labs Zone Description](#create-hearth-labs-zone-description)
    - [Step 1: Set Env for Zone Description Credential](#step-1-set-env-for-zone-description-credential)
    - [Step 2: Key and DID for Zone Credentials](#step-2-key-and-did-for-zone-credentials)
    - [Step 3: Create Zone Description Credential](#step-3-create-zone-description-credential)
    - [Step 4: Sign and Submit Zone Description Credential](#step-4-sign-and-submit-zone-description-credential)
  - [Create Hearth Labs Zone Governance](#create-hearth-labs-zone-governance)
    - [Step 1: Set Env for Zone Governance Credential](#step-1-set-env-for-zone-governance-credential)
    - [Step 2: Create Zone Governance Code](#step-2-create-zone-governance-code)
    - [Step 3: Submit Zone Governance Code](#step-3-submit-zone-governance-code)
    - [Step 4: Create Zone Governance Credential](#step-4-create-zone-governance-credential)
    - [Step 5: Sign and Submit Zone Governance Credential](#step-5-sign-and-submit-zone-governance-credential)
    - [Step 7: Testing Zone Governance Code](#step-7-testing-zone-governance-code)
  - [Querying Cognitarium](#querying-cognitarium)

## Set Env

```bash
# Working directory for Axone files
export WORK_DIR_AXONE=/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned
export FILES_DIR=$WORK_DIR_AXONE/files
export LOG_PATH=$WORK_DIR_AXONE/logs

mkdir -p $LOG_PATH
# mkdir -p $FILES_DIR/{descriptions,governance,auth,publications}

# Path to Axone daemon binary
export AXONED_PATH=/home/manuelpadilla/sources/reposUbuntu/AXONE/tools/axoned-10.0.0-linux-amd64/axoned

# Load helper functions
source $WORK_DIR_AXONE/scripts/helpers.sh

# Axone ontology version
export ONTOLOGY_MAYOR_VERSION=v4
export ONTOLOGY_NEXT_VERSION=vnext

# Node connection
export AXONE_NODE_RPC=https://api.dentrite.axone.xyz:443/rpc
export AXONE_NODE_GRPC=axone-testnet-grpc.polkachu.com:17690
# export AXONE_NODE_GRPC=grpc.dentrite.axone.xyz:443


# export AXONE_NODE_RPC=http://127.0.0.1:26657
# export AXONE_NODE_GRPC=localhost:9090

export NETWORK=$(curl $AXONE_NODE_RPC/status | jq -r '.result.node_info.network')

# Install grpcurl
# curl -sSL "https://github.com/fullstorydev/grpcurl/releases/download/v1.9.3/grpcurl_1.9.3_linux_x86_64.tar.gz" | sudo tar -xz -C /usr/local/bin

# test grp
grpcurl -plaintext $AXONE_NODE_GRPC list
grpcurl -plaintext $AXONE_NODE_GRPC list cosmwasm.wasm.v1.Query
grpcurl -plaintext $AXONE_NODE_GRPC describe cosmwasm.wasm.v1.Query.SmartContractState

# # Create base64 encoded query data first
# export QUERY_DATA=$(echo -n '{"dataverse":{}}' | base64 -w 0)
# # Create the full request JSON with the base64 encoded query_data
# cat > request.json << EOF
# {
#   "address": "axone1uvqk5vj9vn4gjemrp0myz4ku49aaemulgaqw7pfe0nuvfwp3gukqxf4l4g",
#   "query_data": "$QUERY_DATA"
# }
# EOF

# # Send request
# grpcurl -plaintext -d @ \
#   $AXONE_NODE_GRPC cosmwasm.wasm.v1.Query/SmartContractState < request.json

# # Then use the file with grpcurl
# grpcurl -plaintext -d @ $AXONE_NODE_GRPC cosmwasm.wasm.v1.Query/SmartContractState < request.json

# # Create base64 encoded query data
# export QUERY_DATA=$(echo -n '{"dataverse":{}}' | base64 -w 0)

# # Use URL-safe base64 by replacing characters
# export QUERY_DATA_URL_SAFE=$(echo "$QUERY_DATA" | tr '+/' '-_' | tr -d '=')

# # Use the get endpoint format
# grpcurl -plaintext \
#   "$AXONE_NODE_GRPC/cosmwasm/wasm/v1/contract/axone1uvqk5vj9vn4gjemrp0myz4ku49aaemulgaqw7pfe0nuvfwp3gukqxf4l4g/smart/$QUERY_DATA_URL_SAFE"

# Contract Codes

export CODE_ID_DATAVERSE=4
export CODE_ID_COGNITARIUM=3
export CODE_ID_LAW_STONE=2
export CODE_ID_OBJECTARIUM=1

# Keyring backend setting
export KEYRING_BACKEND="--keyring-backend os"

# Logs
echo "" 
echo "" >> $LOG_PATH/implementation.log
echo "Implementation started at $(date)" 
echo "Implementation started at $(date)" >> $LOG_PATH/implementation.log


echo "WORK_DIR_AXONE=\"$WORK_DIR_AXONE\""
echo "WORK_DIR_AXONE=\"$WORK_DIR_AXONE\"" >> $LOG_PATH/implementation.log
echo "FILES_DIR=\"$FILES_DIR\""
echo "FILES_DIR=\"$FILES_DIR\"" >> $LOG_PATH/implementation.log
echo "LOG_PATH=\"$LOG_PATH\""
echo "LOG_PATH=\"$LOG_PATH\"" >> $LOG_PATH/implementation.log
echo "AXONED_PATH=\"$AXONED_PATH\""
echo "AXONED_PATH=\"$AXONED_PATH\"" >> $LOG_PATH/implementation.log
echo "ONTOLOGY_MAYOR_VERSION=\"$ONTOLOGY_MAYOR_VERSION\""
echo "ONTOLOGY_MAYOR_VERSION=\"$ONTOLOGY_MAYOR_VERSION\"" >> $LOG_PATH/implementation.log
echo "ONTOLOGY_NEXT_VERSION=\"$ONTOLOGY_NEXT_VERSION\""
echo "ONTOLOGY_NEXT_VERSION=\"$ONTOLOGY_NEXT_VERSION\"" >> $LOG_PATH/implementation.log
echo "AXONE_NODE_RPC=\"$AXONE_NODE_RPC\""
echo "AXONE_NODE_RPC=\"$AXONE_NODE_RPC\"" >> $LOG_PATH/implementation.log
echo "AXONE_NODE_GRPC=\"$AXONE_NODE_GRPC\""
echo "AXONE_NODE_GRPC=\"$AXONE_NODE_GRPC\"" >> $LOG_PATH/implementation.log

echo "NETWORK=\"$NETWORK\""
echo "NETWORK=\"$NETWORK\"" >> $LOG_PATH/implementation.log

echo "KEYRING_BACKEND=\"$KEYRING_BACKEND\""
echo "KEYRING_BACKEND=\"$KEYRING_BACKEND\"" >> $LOG_PATH/implementation.log

echo "CODE_ID_DATAVERSE=\"$CODE_ID_DATAVERSE\""
echo "CODE_ID_DATAVERSE=\"$CODE_ID_DATAVERSE\"" >> $LOG_PATH/implementation.log
echo "CODE_ID_COGNITARIUM=\"$CODE_ID_COGNITARIUM\""
echo "CODE_ID_COGNITARIUM=\"$CODE_ID_COGNITARIUM\"" >> $LOG_PATH/implementation.log
echo "CODE_ID_LAW_STONE=\"$CODE_ID_LAW_STONE\""
echo "CODE_ID_LAW_STONE=\"$CODE_ID_LAW_STONE\"" >> $LOG_PATH/implementation.log
echo "CODE_ID_OBJECTARIUM=\"$CODE_ID_OBJECTARIUM\""
echo "CODE_ID_OBJECTARIUM=\"$CODE_ID_OBJECTARIUM\"" >> $LOG_PATH/implementation.log
```

## Set Up Issuer Wallet

### Create Issuer Wallet

```bash

# Create DoctAi Issuer Wallet

export ISSUER_WALLET=doctai-issuer-wallet

$AXONED_PATH keys add $ISSUER_WALLET $KEYRING_BACKEND

- address: axone1l3tjnxllfmy4p3082vd388jp8j9m72ypeynm9y
  name: doctai-issuer-wallet
  pubkey: '{"@type":"/cosmos.crypto.secp256k1.PubKey","key":"AgmNFhZ4l9qJ9y0sHnV7QqOa2GxJZAFevkFDSJOzp+2i"}'
  type: local

**Important** write this mnemonic phrase in a safe place.
It is the only way to recover your account if you ever forget your password.

domain rebel sad submit episode equal salad crystal dog example mutual enough onion one summer flat wealth huge pet loyal dance canyon siege stomach

# DoctAi Issuer Wallet configuration 

export ISSUER_ADDRESS=$($AXONED_PATH keys show $ISSUER_WALLET -a $KEYRING_BACKEND)
export ISSUER_DID=$($AXONED_PATH keys show $ISSUER_WALLET -k $KEYRING_BACKEND)

echo "ISSUER_WALLET=\"$ISSUER_WALLET\""
echo "ISSUER_WALLET=\"$ISSUER_WALLET\"" >> $LOG_PATH/implementation.log
echo "ISSUER_ADDRESS=\"$ISSUER_ADDRESS\""
echo "ISSUER_ADDRESS=\"$ISSUER_ADDRESS\"" >> $LOG_PATH/implementation.log
echo "ISSUER_DID=\"$ISSUER_DID\""
echo "ISSUER_DID=\"$ISSUER_DID\"" >> $LOG_PATH/implementation.log
```

### Get AXONE Tokens

Make sure it has balance.

Follow:  

<https://docs.axone.xyz/tutorials/keplr-1>  

Faucet (Request funds with AXONE chain address):  

<https://faucet.axone.xyz/>

### Check Issuer Wallet Balance

```bash
$AXONED_PATH query bank balances $ISSUER_ADDRESS --node $AXONE_NODE_RPC
```

```bash
balances:
- amount: "2929782"
  denom: uaxone
pagination:
  total: "1"
```

```bash
$AXONED_PATH query auth account $ISSUER_ADDRESS --node $AXONE_NODE_RPC
```

```bash
account:
  type: cosmos-sdk/BaseAccount
  value:
    account_number: "2343"
    address: axone1zyef5q2fqx9rgsnyfwc3w9ukfeyltl4hnn2ydg
    public_key:
      type: tendermint/PubKeySecp256k1
      value: AqjA9phopOiUuNgHsnMvoRYE8jZxcE0ozrWoNryEo2x5
    sequence: "1"
```

### Get Account Number and Sequence

```bash
# Account details for transactions
export ISSUER_ACCOUNT_NUMBER=$($AXONED_PATH query auth account $ISSUER_ADDRESS --node $AXONE_NODE_RPC -o json | jq -r '.account.value.account_number')
export ISSUER_SEQUENCE=$($AXONED_PATH query auth account $ISSUER_ADDRESS --node $AXONE_NODE_RPC -o json | jq -r '.account.value.sequence')

echo "ISSUER_ACCOUNT_NUMBER=\"$ISSUER_ACCOUNT_NUMBER\""
echo "ISSUER_ACCOUNT_NUMBER=\"$ISSUER_ACCOUNT_NUMBER\"" >> $LOG_PATH/implementation.log
echo "ISSUER_SEQUENCE=\"$ISSUER_SEQUENCE\""
echo "ISSUER_SEQUENCE=\"$ISSUER_SEQUENCE\"" >> $LOG_PATH/implementation.log
```

### Verify Generated DIDs

Get all created keys:

```bash
$AXONED_PATH keys list $KEYRING_BACKEND
```

## Set Up Smart Contracts

### Objectarium

```bash
export OBJECTARIUM_LABEL="my-objectarium"
echo "OBJECTARIUM_LABEL=\"$OBJECTARIUM_LABEL\""
echo "OBJECTARIUM_LABEL=\"$OBJECTARIUM_LABEL\"" >> $LOG_PATH/implementation.log

export OBJECTARIUM_TX_HASH=$($AXONED_PATH tx wasm instantiate $CODE_ID_OBJECTARIUM \
    "{\"bucket\":\"$OBJECTARIUM_LABEL\"}" \
    --label "$OBJECTARIUM_LABEL" \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from "$ISSUER_ADDRESS" \
    --admin "$ISSUER_ADDRESS" \
    --gas 1000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$OBJECTARIUM_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

export OBJECTARIUM_ADDR=$($AXONED_PATH query tx $OBJECTARIUM_TX_HASH --node $AXONE_NODE_RPC -o json | \
    jq -r '.events[] | select(.type == "instantiate") | .attributes[] | select(.key == "_contract_address") | .value')

echo "OBJECTARIUM_TX_HASH=\"$OBJECTARIUM_TX_HASH\"" 
echo "OBJECTARIUM_TX_HASH=\"$OBJECTARIUM_TX_HASH\"" >> $LOG_PATH/implementation.log
echo "OBJECTARIUM_ADDR=\"$OBJECTARIUM_ADDR\"" 
echo "OBJECTARIUM_ADDR=\"$OBJECTARIUM_ADDR\"" >> $LOG_PATH/implementation.log
```

### Dataverse

```bash
export DATAVERSE_LABEL="my-dataverse"
echo "DATAVERSE_LABEL=\"$DATAVERSE_LABEL\"" 
echo "DATAVERSE_LABEL=\"$DATAVERSE_LABEL\"" >> $LOG_PATH/implementation.log

export DATAVERSE_TX_HASH=$($AXONED_PATH tx wasm instantiate $CODE_ID_DATAVERSE \
    "{\"name\":\"$DATAVERSE_LABEL\",\"triplestore_config\":{\"code_id\":\"$CODE_ID_COGNITARIUM\",\"limits\":{}}}" \
    --label "$DATAVERSE_LABEL" \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from "$ISSUER_ADDRESS" \
    --admin "$ISSUER_ADDRESS" \
    --gas 1000000 \
   --yes -o json | jq -r '.txhash')

wait_and_check_tx "$DATAVERSE_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

export DATAVERSE_ADDR=$($AXONED_PATH query tx $DATAVERSE_TX_HASH --node $AXONE_NODE_RPC -o json | \
    jq --arg CODE_ID "$CODE_ID_DATAVERSE" -r ' 
        .events[] 
        | select(.type == "instantiate") 
        | select(.attributes[] | select(.key == "code_id" and .value == $CODE_ID)) 
        | .attributes[] 
        | select(.key == "_contract_address") 
        | .value')

export COGNITARIUM_ADDR=$($AXONED_PATH query tx $DATAVERSE_TX_HASH --node $AXONE_NODE_RPC -o json | \
    jq --arg CODE_ID "$CODE_ID_COGNITARIUM" -r '
        .events[] 
        | select(.type == "instantiate") 
        | select(.attributes[] | select(.key == "code_id" and .value == $CODE_ID)) 
        | .attributes[] 
        | select(.key == "_contract_address")
        | .value')

echo "DATAVERSE_TX_HASH=\"$DATAVERSE_TX_HASH\"" 
echo "DATAVERSE_TX_HASH=\"$DATAVERSE_TX_HASH\"" >> $LOG_PATH/implementation.log
echo "DATAVERSE_ADDR=\"$DATAVERSE_ADDR\""
echo "DATAVERSE_ADDR=\"$DATAVERSE_ADDR\"" >> $LOG_PATH/implementation.log
echo "COGNITARIUM_ADDR=\"$COGNITARIUM_ADDR\"" 
echo "COGNITARIUM_ADDR=\"$COGNITARIUM_ADDR\"" >> $LOG_PATH/implementation.log

# Check if COGNITARIUM_ADDR is ok
$AXONED_PATH --node $AXONE_NODE_RPC query wasm contract-state smart \
    $DATAVERSE_ADDR \
    '{"dataverse":{}}' \
    -o json \
    | jq -r '.data.triplestore_address'

# whitelist & blacklist predicates
$AXONED_PATH --node $AXONE_NODE_RPC query logic params -o json \
  | jq -r '.params.interpreter.predicates_filter | "whitelist: " + (.whitelist | join(", ")) + "\nblacklist: " + (.blacklist | join(", "))'

# Verify all DIDs
$AXONED_PATH keys list $KEYRING_BACKEND
```

## Create Hearth Labs Zone Description

### Step 1: Set Env for Zone Description Credential

```bash
export ZONE_DESC_PATH=$WORK_DIR_AXONE/files/zones/description
mkdir -p $ZONE_DESC_PATH

export ZONE_WALLET=hearth-labs-wallet

echo "ZONE_DESC_PATH=\"$ZONE_DESC_PATH\""
echo "ZONE_DESC_PATH=\"$ZONE_DESC_PATH\"" >> $LOG_PATH/implementation.log
echo "ZONE_WALLET=\"$ZONE_WALLET\""
echo "ZONE_WALLET=\"$ZONE_WALLET\"" >> $LOG_PATH/implementation.log

```

### Step 2: Key and DID for Zone Credentials

Create keys for the Hearth Labs zone:

```bash
$AXONED_PATH keys add $ZONE_WALLET $KEYRING_BACKEND
```

```bash
- address: axone1ukpvhk9jmgmqktmcph23eyr0q3pwae0tl86sqz
  name: hearth-labs-wallet
  pubkey: '{"@type":"/cosmos.crypto.secp256k1.PubKey","key":"A2HH2XzqGbpDM8j/u4HsaiilgfYt08uaLKDknR9EX+8l"}'
  type: local


**Important** write this mnemonic phrase in a safe place.
It is the only way to recover your account if you ever forget your password.

silly inflict hungry inhale brush avocado sample ivory swarm audit add tattoo brain diagram enemy obvious submit garbage flee media donate bird sail vocal
```

```bash
# Get zone DID
export ZONE_ADDRESS=$($AXONED_PATH keys show $ZONE_WALLET -a $KEYRING_BACKEND)
export ZONE_DID=$($AXONED_PATH keys show $ZONE_WALLET -k $KEYRING_BACKEND)


echo "ZONE_ADDRESS=\"$ZONE_ADDRESS\""
echo "ZONE_ADDRESS=\"$ZONE_ADDRESS\"" >> $LOG_PATH/implementation.log
echo "ZONE_DID=\"$ZONE_DID\""
echo "ZONE_DID=\"$ZONE_DID\"" >> $LOG_PATH/implementation.log
```

### Step 3: Create Zone Description Credential

[Schema](https://docs.axone.xyz/ontology/schemas/credential-zone-description)

Create the zone description using the credential-zone-description template:

```bash
export ZONE_CRED_DESCRIPTION_ID=$(uuidgen)
echo "ZONE_CRED_DESCRIPTION_ID=\"$ZONE_CRED_DESCRIPTION_ID\""
echo "ZONE_CRED_DESCRIPTION_ID=\"$ZONE_CRED_DESCRIPTION_ID\"" >> $LOG_PATH/implementation.log

cat <<EOF | envsubst > $ZONE_DESC_PATH/$ZONE_WALLET-description.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/zone/description/"
  ],
  "type": ["VerifiableCredential", "ZoneDescriptionCredential"],
  "id": "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/zone/description/$ZONE_CRED_DESCRIPTION_ID",
  "credentialSubject": {
    "id": "$ZONE_DID",
    "hasDescription": "Hearth Labs is a governed zone focused on collaborative analysis of cardiac data through AI agents.",
    "hasTag": [
      "Health",
      "AI",
      "Cardiology"
    ],
    "hasTitle": "Hearth Labs",
    "hasTopic": "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/thesaurus/topic/health"
  },
  "issuanceDate": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "issuer": {
    "id": "$ISSUER_DID",
    "name": "$ISSUER_WALLET"
  }
}
EOF
```

### Step 4: Sign and Submit Zone Description Credential

Sign and encode the zone credentials:

```bash
# Sign the zone credential
$AXONED_PATH credential sign $ZONE_DESC_PATH/$ZONE_WALLET-description.jsonld \
    $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $ZONE_DESC_PATH/$ZONE_WALLET-description.nq

# Submit zone description to blockchain
export ZONE_CRED_DESCRIPTION_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
    "{\"submit_claims\":{\
        \"claims\": \"$(base64 -w 0 $ZONE_DESC_PATH/$ZONE_WALLET-description.nq)\", \
        \"format\": \"n_quads\" \
    }}" \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --gas 10000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$ZONE_CRED_DESCRIPTION_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "ZONE_CRED_DESCRIPTION_TX_HASH=\"$ZONE_CRED_DESCRIPTION_TX_HASH\"" 
echo "ZONE_CRED_DESCRIPTION_TX_HASH=\"$ZONE_CRED_DESCRIPTION_TX_HASH\"" >> $LOG_PATH/implementation.log
```

## Create Hearth Labs Zone Governance

### Step 1: Set Env for Zone Governance Credential

```bash
export ZONE_GOV_PATH=$WORK_DIR_AXONE/files/zones/governance
mkdir -p $ZONE_GOV_PATH

echo "ZONE_GOV_PATH=\"$ZONE_GOV_PATH\""
echo "ZONE_GOV_PATH=\"$ZONE_GOV_PATH\"" >> $LOG_PATH/implementation.log
```

### Step 2: Create Zone Governance Code

```bash
# Create service governance Prolog rules
cat <<EOF | envsubst > $ZONE_GOV_PATH/$ZONE_WALLET-governance.pl
% Hearth Labs Zone Governance
% ===========================

:- discontiguous([permitted_action/2, title/2, partOf/2, chapter/1, section/1, article/1, paragraph/2]).

% Metadata
chapter('chap1').
title('chap1', 'Hearth Labs Governance').

% Static rules
% ------------------

% Only this DID can update the zone
permitted_action('$ISSUER_DID', 'zone:update').

% Contributors that can register resources
contributor_id('did:key:z6DtrNpHS569CKDUFaXGzAMnLdMoaanxHqqsPnd91ZnyufLL').

permitted_action(DID, 'resource:register') :- contributor_id(DID).

% Query predicates
tell_permitted_actions(DID, Actions) :-
    findall(Action, permitted_action(DID, Action), Actions).

tell(DID, Action, permitted, 'default') :-
    permitted_action(DID, Action), !.
tell(_, _, denied, 'default').
EOF
```

### Step 3: Submit Zone Governance Code

Create a Law Stone contract instance for the zone governance:

```bash
# Generate unique label
export ZONE_GOV_CODE_ID=$(uuidgen)
echo "ZONE_GOV_CODE_ID=\"$ZONE_GOV_CODE_ID\""
echo "ZONE_GOV_CODE_ID=\"$ZONE_GOV_CODE_ID\"" >> $LOG_PATH/implementation.log

# Submit program
export ZONE_GOV_CODE_TX_HASH=$($AXONED_PATH tx wasm instantiate $CODE_ID_LAW_STONE \
    "{\"program\":\"$(base64 -w 0 $ZONE_GOV_PATH/$ZONE_WALLET-governance.pl)\", \"storage_address\": \"$OBJECTARIUM_ADDR\"}" \
    --label $ZONE_GOV_CODE_ID \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --admin $ISSUER_ADDRESS \
    --gas 20000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$ZONE_GOV_CODE_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "ZONE_GOV_CODE_TX_HASH=\"$ZONE_GOV_CODE_TX_HASH\"" 
echo "ZONE_GOV_CODE_TX_HASH=\"$ZONE_GOV_CODE_TX_HASH\"" >> $LOG_PATH/implementation.log

# Get contract address
export ZONE_GOV_CODE_ADDR=$($AXONED_PATH query tx $ZONE_GOV_CODE_TX_HASH --node $AXONE_NODE_RPC -o json | \
    jq -r '.events[] | select(.type == "instantiate") | .attributes[] | select(.key == "_contract_address") | .value')

echo "ZONE_GOV_CODE_ADDR=\"$ZONE_GOV_CODE_ADDR\""
echo "ZONE_GOV_CODE_ADDR=\"$ZONE_GOV_CODE_ADDR\"" >> $LOG_PATH/implementation.log
```

### Step 4: Create Zone Governance Credential

Create the governance credential file:

```bash
export ZONE_CRED_GOV_ID=$(uuidgen)
echo "ZONE_CRED_GOV_ID=\"$ZONE_CRED_GOV_ID\""
echo "ZONE_CRED_GOV_ID=\"$ZONE_CRED_GOV_ID\"" >> $LOG_PATH/implementation.log

cat <<EOF | envsubst > $ZONE_GOV_PATH/$ZONE_WALLET-governance-credential.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/governance/text/"
  ],
  "type": ["VerifiableCredential", "GovernanceTextCredential"],
  "id": "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/governance/text/$ZONE_CRED_GOV_ID",
  "credentialSubject": {
    "id": "$ZONE_DID",
    "isGovernedBy": {
      "type": "GovernanceText",
      "fromGovernance": "cosmwasm:law-stone:${ZONE_GOV_CODE_ADDR}?query=%22program_code%22"
    }
  },
  "issuanceDate": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "issuer": {
    "id": "$ISSUER_DID",
    "name": "$ISSUER_WALLET"
  }
}
EOF
```

### Step 5: Sign and Submit Zone Governance Credential

```bash
# Sign the credential
$AXONED_PATH credential sign $ZONE_GOV_PATH/$ZONE_WALLET-governance-credential.jsonld \
  $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $ZONE_GOV_PATH/$ZONE_WALLET-governance-credential.nq

# Encode inline and submit directly
export ZONE_CRED_GOV_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
  "{\"submit_claims\":{\
    \"claims\": \"$(base64 -w 0 $ZONE_GOV_PATH/$ZONE_WALLET-governance-credential.nq)\", \
    \"format\": \"n_quads\" \
  }}" \
  --node $AXONE_NODE_RPC \
  --chain-id $NETWORK \
  $KEYRING_BACKEND --from $ISSUER_ADDRESS \
  --account-number $ISSUER_ACCOUNT_NUMBER \
  --sequence $ISSUER_SEQUENCE \
  --gas 10000000 \
  --yes -o json | jq -r '.txhash')

# Confirm on chain
wait_and_check_tx "$ZONE_CRED_GOV_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "ZONE_CRED_GOV_TX_HASH=\"$ZONE_CRED_GOV_TX_HASH\"" 
echo "ZONE_CRED_GOV_TX_HASH=\"$ZONE_CRED_GOV_TX_HASH\"" >> $LOG_PATH/implementation.log
```

### Step 7: Testing Zone Governance Code

```bash
# Identidad a testear
# export TEST_DID=$ISSUER_DID            # o cualquier DID válido
export TEST_DID1="did:key:z16DtrNpHS569CKDUFaXGzAMnLdMoaanxHqqsPnd91ZnyufLL"
export TEST_DID2="did:key:z6DtrNpHS569CKDUFaXGzAMnLdMoaanxHqqsPnd91ZnyufLL"

# export TEST_ACTION="zone:update"     # acción a verificar
export TEST_ACTION="resource:register"     # acción a verificar

# Contrato law-stone
export LAW_STONE_ADDR=$ZONE_GOV_CODE_ADDR

# ------------------------------
# ASK via gRPC (tell_permitted_actions)
# ------------------------------

export PROLOG_QUERY="tell_permitted_actions('$TEST_DID2', Actions)."
export ENCODED_QUERY=$(echo -n "{\"ask\":{\"query\":\"$PROLOG_QUERY\"}}" | base64 -w 0)

echo "🔍 GRPC tell_permitted_actions for DID=\"$TEST_DID2\""

export RESULT_GRPC=$(grpcurl -plaintext -d @ \
  $AXONE_NODE_GRPC cosmwasm.wasm.v1.Query/SmartContractState <<EOF
{
  "address": "$LAW_STONE_ADDR",
  "query_data": "$ENCODED_QUERY"
}
EOF
)

# Extraer y decodificar el campo 'data'
echo "$RESULT_GRPC" | jq -r '.data' | base64 -d | jq

# ------------------------------
# ASK via AXONED (tell/4)
# ------------------------------

export ASK_QUERY="tell('$TEST_DID', '$TEST_ACTION', Result, Evidence)."

echo ""
echo "🔍 AXONED tell('$TEST_DID', '$TEST_ACTION', Result, Evidence):"

export RESULT_RPC=$($AXONED_PATH query wasm contract-state smart $LAW_STONE_ADDR \
  --node $AXONE_NODE_RPC \
  -o json \
  "{\"ask\":{\"query\":\"$ASK_QUERY\"}}")

# Mostrar formateado
echo "$RESULT_RPC" | jq
```

## Querying Cognitarium

```bash
# ------------------------------
# ASK ALL COGNITARIUM TRIPLES
# ------------------------------

QUERY=$(jq -n '{
  select: {
    query: {
    "prefixes": [],
      select: [
        { variable: "s" },
        { variable: "p" },
        { variable: "o" }
      ],
      where: {
        bgp: {
          patterns: [
            {
              subject: { variable: "s" },
              predicate: { variable: "p" },
              object: { variable: "o" }
            }
          ]
        }
      }
    }
  }
}')


$AXONED_PATH query wasm contract-state smart $COGNITARIUM_ADDR "$QUERY" --node $AXONE_NODE_RPC --output json | jq

# ------------------------------
# GET ALL ZONES
# ------------------------------

$AXONED_PATH --node $AXONE_NODE_RPC query wasm contract-state smart  \
    $COGNITARIUM_ADDR \
    '{"select":{"query":{"prefixes":[],"select":[{"variable":"zone"}],"where":{"bgp":{"patterns":[{"subject":{"variable":"credential"},"predicate":{"named_node":{"full":"dataverse:credential:body#subject"}},"object":{"variable":"zone"}},{"subject":{"variable":"credential"},"predicate":{"named_node":{"full":"dataverse:credential:body#type"}},"object":{"node":{"named_node":{"full":"https://w3id.org/axone/ontology/v4/schema/credential/zone/description/ZoneDescriptionCredential"}}}}]}}}}}' \
    -o json \
    | jq -r '.data.results.bindings[0].zone.value.full'

# ------------------------------
# GET ZONE DESCRIPTION CREDENTIAL
# ------------------------------

QUERY=$(jq -n --arg did "$ZONE_DID" '{"select":{"query":{"prefixes":[],"select":[{"variable":"p"},{"variable":"o"}],"where":{"lateral_join":{"left":{"bgp":{"patterns":[{"subject":{"variable":"credential"},"predicate":{"named_node":{"full":"dataverse:credential:body#subject"}},"object":{"node":{"named_node":{"full": $did}}}},{"subject":{"variable":"credential"},"predicate":{"named_node":{"full":"dataverse:credential:body#type"}},"object":{"node":{"named_node":{"full":"https://w3id.org/axone/ontology/v4/schema/credential/zone/description/ZoneDescriptionCredential"}}}}]}},"right":{"bgp":{"patterns":[{"subject":{"variable":"credential"},"predicate":{"variable":"dataverse:credential:body#claim"},"object":{"variable":"claim"}},{"subject":{"variable":"claim"},"predicate":{"variable":"p"},"object":{"variable":"o"}}]}}}}}}}')

$AXONED_PATH --node $AXONE_NODE_RPC query wasm contract-state smart \
    $COGNITARIUM_ADDR \
    "$QUERY" \
    -o json \
    | jq '.data.results.bindings'

# ------------------------------
# GET ZONE GOV CREDENTIAL
# ------------------------------

QUERY=$(jq -n --arg did "$ZONE_DID" '{"select":{"query":{"prefixes":[],"select":[{"variable":"p"},{"variable":"o"}],"where":{"lateral_join":{"left":{"bgp":{"patterns":[{"subject":{"variable":"credential"},"predicate":{"named_node":{"full":"dataverse:credential:body#subject"}},"object":{"node":{"named_node":{"full": $did}}}},{"subject":{"variable":"credential"},"predicate":{"named_node":{"full":"dataverse:credential:body#type"}},"object":{"node":{"named_node":{"full":"https://w3id.org/axone/ontology/v4/schema/credential/governance/text/GovernanceTextCredential"}}}},{"subject":{"variable":"credential"},"predicate":{"variable":"dataverse:credential:body#claim"},"object":{"variable":"claim"}},{"subject":{"variable":"claim"},"predicate":{"named_node":{"full":"https://w3id.org/axone/ontology/v4/schema/credential/governance/text/isGovernedBy"}},"object":{"variable":"governance"}}]}},"right":{"bgp":{"patterns":[{"subject":{"variable":"governance"},"predicate":{"variable":"p"},"object":{"variable":"o"}}]}}}}}}}')

$AXONED_PATH --node $AXONE_NODE_RPC query wasm contract-state smart \
    $COGNITARIUM_ADDR \
    "$QUERY" \
    -o json \
    | jq '.data.results.bindings'

# ------------------------------
# GET ZONE GOV CREDENTIAL FROM ZONE DID
# ------------------------------

QUERY=$(jq -n --arg did "$ZONE_DID" '{
  select: {
    query: {
      prefixes: [],
      select: [
        { variable: "credential" },
        { variable: "p" },
        { variable: "o" }
      ],
      where: {
        lateral_join: {
          left: {
            bgp: {
              patterns: [
                {
                  subject: { variable: "credential" },
                  predicate: { named_node: { full: "dataverse:credential:body#subject" } },
                  object: { node: { named_node: { full: $did } } }
                },
                {
                  subject: { variable: "credential" },
                  predicate: { named_node: { full: "dataverse:credential:body#type" } },
                  object: { node: { named_node: { full: "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/GovernanceTextCredential" } } }
                },
                {
                  subject: { variable: "credential" },
                  predicate: { variable: "dataverse:credential:body#claim" },
                  object: { variable: "claim" }
                },
                {
                  subject: { variable: "claim" },
                  predicate: { named_node: { full: "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/isGovernedBy" } },
                  object: { variable: "governance" }
                }
              ]
            }
          },
          right: {
            bgp: {
              patterns: [
                {
                  subject: { variable: "governance" },
                  predicate: { variable: "p" },
                  object: { variable: "o" }
                }
              ]
            }
          }
        }
      }
    }
  }
}')

$AXONED_PATH query wasm contract-state smart $COGNITARIUM_ADDR "$QUERY" --node $AXONE_NODE_RPC --output json | jq

# ------------------------------
# GET ZONE GOV CODE
# ------------------------------

$AXONED_PATH --node $AXONE_NODE_RPC query wasm contract-state smart \
  $ZONE_GOV_CODE_ADDR \
  '{"program_code":{}}' -o json | jq -r '.data' | base64 -d

```
