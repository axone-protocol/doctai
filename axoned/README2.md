
# README MINIO STORAGE SERVICE

## Table of Contents

- [README MINIO STORAGE SERVICE](#readme-minio-storage-service)
  - [Table of Contents](#table-of-contents)
  - [Set Up Enviromental Variables](#set-up-enviromental-variables)
  - [Set Up MinIO Wallet](#set-up-minio-wallet)
    - [Create MinIO Wallet](#create-minio-wallet)
    - [Get AXONE Tokens](#get-axone-tokens)
    - [Check MinIO Wallet Balance](#check-minio-wallet-balance)
  - [Create MinIO S3 Storage Service Description](#create-minio-s3-storage-service-description)
    - [Step 1: Set Up Enviromental Variables for Service Description Credential](#step-1-set-up-enviromental-variables-for-service-description-credential)
    - [Step 2: Create Service Description Credential](#step-2-create-service-description-credential)
    - [Step 3: Sign and Submit Service Description Credential](#step-3-sign-and-submit-service-description-credential)
  - [Create MinIO S3 Storage Service Governance](#create-minio-s3-storage-service-governance)
    - [Step 1: Set Enviromental Variables for Service Governance Credential](#step-1-set-enviromental-variables-for-service-governance-credential)
    - [Step 2: Create Service Governance Code](#step-2-create-service-governance-code)
    - [Step 3: Submit Service Governance Code](#step-3-submit-service-governance-code)
    - [Step 4: Create Service Governance Credentials](#step-4-create-service-governance-credentials)
    - [Step 5: Sign and Submit Service Governance Credentials](#step-5-sign-and-submit-service-governance-credentials)
    - [Step 7: Testing Service Governance Code](#step-7-testing-service-governance-code)
  - [Create MinIO S3 Storage Service Zone Membership](#create-minio-s3-storage-service-zone-membership)
    - [Step 1: Create Service Zone Membership Credential](#step-1-create-service-zone-membership-credential)
    - [Step 4: Sign and Submit Service Zone Membership Credential](#step-4-sign-and-submit-service-zone-membership-credential)
  - [Querying Cognitarium](#querying-cognitarium)
    - [Get All Triples](#get-all-triples)
    - [Get MinIO Gov Address](#get-minio-gov-address)
    - [Get Zone DID from MinIO Service DID using Zone Membership Credential](#get-zone-did-from-minio-service-did-using-zone-membership-credential)
    - [Get Credential From DID](#get-credential-from-did)

## Set Up Enviromental Variables

Use exported variables in [../logs/implementation.log] to set up the environment for the MinIO S3 Storage Service. This includes paths for the Axone node, wallet, and other necessary configurations.

```bash
WORK_DIR_AXONE="/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned"
FILES_DIR="/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/files"
LOG_PATH="/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/logs"
AXONED_PATH="/home/manuelpadilla/sources/reposUbuntu/AXONE/tools/axoned-10.0.0-linux-amd64/axoned"
ONTOLOGY_MAYOR_VERSION="v4"
ONTOLOGY_NEXT_VERSION="vnext"
AXONE_NODE_RPC="https://api.dentrite.axone.xyz:443/rpc"
AXONE_NODE_GRPC="axone-testnet-grpc.polkachu.com:17690"
NETWORK="axone-dentrite-1"
KEYRING_BACKEND="--keyring-backend os"
CODE_ID_DATAVERSE="4"
CODE_ID_COGNITARIUM="3"
CODE_ID_LAW_STONE="2"
CODE_ID_OBJECTARIUM="1"
ISSUER_WALLET="doctai-issuer-wallet"
ISSUER_ADDRESS="axone1l3tjnxllfmy4p3082vd388jp8j9m72ypeynm9y"
ISSUER_DID="did:key:zQ3shN45CkRnjackWYwM191q2bNanAYeP1ZvxoAEPaV4ZpgZo"
ISSUER_ACCOUNT_NUMBER="47013"
ISSUER_SEQUENCE="23"
OBJECTARIUM_LABEL="my-objectarium"
OBJECTARIUM_TX_HASH="B352742121F31C29F71A6B16A665436B4964BCF361541D6BCDEBCD3C6787EFAB"
OBJECTARIUM_ADDR="axone1w9udhqtvuefwfmx7af9qe5jjhmrjk4p7ezxm6py08puykgen6uhqfsmuh3"
DATAVERSE_LABEL="my-dataverse"
DATAVERSE_TX_HASH="2333D6B42078E7FF9C245ACB3016ED0542F6D8D04F655FC9487F6A94D1CE13A8"
DATAVERSE_ADDR="axone1nm3yktzcgpnvwu6qpzqgl2ktyvlgsstc7ev849dd3ulaygw75mqq6saarw"
COGNITARIUM_ADDR="axone1mhzpd0yhdyhn8kzgznvjmrzrc7f9ksf2jyzzsw4pzjw52uufwc0qgw0h0j"
ZONE_DESC_PATH="/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/files/zones/description"
ZONE_WALLET="hearth-labs-wallet"
ZONE_ADDRESS="axone1ukpvhk9jmgmqktmcph23eyr0q3pwae0tl86sqz"
ZONE_DID="did:key:zQ3shmDoRRiFmNzwDrnV2iPdgA9xaoAYsSq8GC9tGHsNL4R7a"
ZONE_CRED_DESCRIPTION_ID="c3058dd1-5e02-4143-bdd6-916f332fe25c"
ZONE_CRED_DESCRIPTION_TX_HASH="E1D022AC96AAC2EFBE5AE34D2BBD52101152B5857622112B91C96226F729D065"
ZONE_GOV_PATH="/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/files/zones/governance"
ZONE_GOV_CODE_ID="a28141b6-5157-4f84-a787-84b066c7cfce"
ZONE_GOV_CODE_TX_HASH="C05E4E67642774E86360BC5344432121691AE474281904BE352DED1970FCB2B2"
ZONE_GOV_CODE_ADDR="axone1y7cy7cyz3m5ak20u0ten8cahp55k48pcgrwmwzq7rpxfv0qlh6cs5nqtmy"
ZONE_CRED_GOV_ID="2a01daf9-f47c-41a4-be5e-f0e4e636922a"
ZONE_CRED_GOV_TX_HASH="808B22DF602C8B58BF32DA4CEE8B50EA0E17ED6430684AC101017D92E2CCC4E4"

# Load helper functions
source $WORK_DIR_AXONE/scripts/helpers.sh
```

## Set Up MinIO Wallet

### Create MinIO Wallet

```bash

# Create MinIO Issuer Wallet

export MINIO_WALLET=minio-wallet

echo "MINIO_WALLET=\"$MINIO_WALLET\""
echo "MINIO_WALLET=\"$MINIO_WALLET\"" >> $LOG_PATH/implementation.log

$AXONED_PATH keys add $MINIO_WALLET $KEYRING_BACKEND

```

```bash
- address: axone1nxg9q8qv8nyu5yt62c3kqdrsm4fd6earylz2r7
  name: minio-wallet
  pubkey: '{"@type":"/cosmos.crypto.secp256k1.PubKey","key":"AxiMRT+BFUzo+wAbuBMu8A1/UHpTCEzYukWls3PAPH69"}'
  type: local


**Important** write this mnemonic phrase in a safe place.
It is the only way to recover your account if you ever forget your password.

actress outer equal gain brisk ship dawn patch sock culture junior wise banner train panic demise nothing eye tortoise north vibrant people gym shed
```

```bash
# MInIO Issuer Wallet configuration 

# Get minio did o DID
export MINIO_ADDRESS=$($AXONED_PATH keys show $MINIO_WALLET -a $KEYRING_BACKEND)
export MINIO_DID=$($AXONED_PATH keys show $MINIO_WALLET -k $KEYRING_BACKEND)

export MINIO_MNEMONIC_PHRASE="actress outer equal gain brisk ship dawn patch sock culture junior wise banner train panic demise nothing eye tortoise north vibrant people gym shed"

echo "MINIO_ADDRESS=\"$MINIO_ADDRESS\""
echo "MINIO_ADDRESS=\"$MINIO_ADDRESS\"" >> $LOG_PATH/implementation.log
echo "MINIO_DID=\"$MINIO_DID\""
echo "MINIO_DID=\"$MINIO_DID\"" >> $LOG_PATH/implementation.log
echo "MINIO_MNEMONIC_PHRASE=\"$MINIO_MNEMONIC_PHRASE\""
echo "MINIO_MNEMONIC_PHRASE=\"$MINIO_MNEMONIC_PHRASE\"" >> $LOG_PATH/implementation.log
```

### Get AXONE Tokens

Make sure it has balance.

Follow:  

<https://docs.axone.xyz/tutorials/keplr-1>  

Faucet (Request funds with AXONE chain address):  

<https://faucet.axone.xyz/>

### Check MinIO Wallet Balance

```bash
$AXONED_PATH query bank balances $MINIO_ADDRESS --node $AXONE_NODE_RPC
```

```bash
balances:
- amount: "1000000"
  denom: uaxone
pagination:
  total: "1"
```

```bash
$AXONED_PATH query auth account $MINIO_ADDRESS --node $AXONE_NODE_RPC
```

```bash
account:
  type: cosmos-sdk/BaseAccount
  value:
    account_number: "47205"
    address: axone1nxg9q8qv8nyu5yt62c3kqdrsm4fd6earylz2r7
```

## Create MinIO S3 Storage Service Description

### Step 1: Set Up Enviromental Variables for Service Description Credential

```bash
export MINIO_DESC_PATH=$WORK_DIR_AXONE/files/minio/description
mkdir -p $MINIO_DESC_PATH

echo "MINIO_DESC_PATH=\"$MINIO_DESC_PATH\""
echo "MINIO_DESC_PATH=\"$MINIO_DESC_PATH\"" >> $LOG_PATH/implementation.log
```

### Step 2: Create Service Description Credential

```bash
export MINIO_CRED_DESCRIPTION_ID=$(uuidgen)
echo "MINIO_CRED_DESCRIPTION_ID=\"$MINIO_CRED_DESCRIPTION_ID\""
echo "MINIO_CRED_DESCRIPTION_ID=\"$MINIO_CRED_DESCRIPTION_ID\"" >> $LOG_PATH/implementation.log

# Create MinIO Service Description
cat <<EOF | envsubst > $MINIO_DESC_PATH/$MINIO_WALLET-description.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/digital-service/description/"
  ],
  "type": ["VerifiableCredential", "DigitalServiceDescriptionCredential"],
  "id": "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/digital-service/description/$MINIO_CRED_DESCRIPTION_ID",
  "credentialSubject": {
    "id": "$MINIO_DID",
    "hasCategory": "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/thesaurus/digital-service-category/Storage",
    "hasDescription": "MinIO S3",
    "hasPublisher": "Me",
    "hasTag": [
      "Storage",
      "Cloud"
    ],
    "hasTitle": "My MinIO"
  },
  "issuanceDate": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "issuer": {
    "id": "$ISSUER_DID",
    "name": "$ISSUER_WALLET"
  }
}
EOF
```

### Step 3: Sign and Submit Service Description Credential

Sign and encode the zone credentials:

```bash

$AXONED_PATH credential sign $MINIO_DESC_PATH/$MINIO_WALLET-description.jsonld \
    $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $MINIO_DESC_PATH/$MINIO_WALLET-description.nq

# Submit minio description to blockchain
export MINIO_CRED_DESCRIPTION_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
    "{\"submit_claims\":{\
        \"claims\": \"$(base64 -w 0 $MINIO_DESC_PATH/$MINIO_WALLET-description.nq)\", \
        \"format\": \"n_quads\" \
    }}" \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --gas 10000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$MINIO_CRED_DESCRIPTION_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_CRED_DESCRIPTION_TX_HASH=\"$MINIO_CRED_DESCRIPTION_TX_HASH\"" 
echo "MINIO_CRED_DESCRIPTION_TX_HASH=\"$MINIO_CRED_DESCRIPTION_TX_HASH\"" >> $LOG_PATH/implementation.log
```

## Create MinIO S3 Storage Service Governance

### Step 1: Set Enviromental Variables for Service Governance Credential

```bash
export MINIO_GOV_PATH=$WORK_DIR_AXONE/files/minio/governance
mkdir -p $MINIO_GOV_PATH

echo "MINIO_GOV_PATH=\"$MINIO_GOV_PATH\""
echo "MINIO_GOV_PATH=\"$MINIO_GOV_PATH\"" >> $LOG_PATH/implementation.log
```

### Step 2: Create Service Governance Code

```bash
# Create service governance Prolog rules
cat <<EOF | envsubst > $MINIO_GOV_PATH/$MINIO_WALLET-governance.pl
% MinIO S3 Governance
% ===========================

:- discontiguous([title/2, partOf/2, chapter/1, section/1, subSection/1, article/1, paragraph/2]).

% Structure
title('1', 'MinIO Service').
chapter('1').

    title('1.1', 'Service Access Control'). partOf('1.1', '1').
    section('1.1').

        title('1.1.1', 'Public Access'). partOf('1.1.1', '1.1').
        subSection('1.1.1').

            title('1.1.1.1', 'Read'). partOf('1.1.1.1', '1.1.1').
            article('1.1.1.1').

                title('1.1.1.1.1', 'Read is allowed for everyone'). partOf('1.1.1.1.1', '1.1.1.1').
                paragraph('1.1.1.1.1', permitted).

        title('1.1.2', 'Restricted Access'). partOf('1.1.2', '1.1').
        subSection('1.1.2').

            title('1.1.2.1', 'Store'). partOf('1.1.2.1', '1.1.2').
            article('1.1.2.1').

                title('1.1.2.1.1', 'Store is allowed for everyone'). partOf('1.1.2.1.1', '1.1.2.1').
                paragraph('1.1.2.1.1', permitted).

            title('1.1.2.2', 'Validate File Types'). partOf('1.1.2.2', '1.1.2').
            article('1.1.2.2').

                paragraph('1.1.2.2.1', permitted).
                paragraph('1.1.2.2.2', permitted).

            title('1.1.2.3', 'Validate File Sizes'). partOf('1.1.2.3', '1.1.2').
            article('1.1.2.3').

                title('1.1.2.3.1', 'Maximum file size is 50MB'). partOf('1.1.2.3.1', '1.1.2.3').
                paragraph('1.1.2.3.1', permitted).

        title('1.1.3', 'Governance'). partOf('1.1.3', '1.1').
        subSection('1.1.3').

            title('1.1.3.1', 'Change'). partOf('1.1.3.1', '1.1.3').
            article('1.1.3.1').

                paragraph('1.1.3.1.1', permitted).

% File size helpers
max_file_size('52428800').

trim_leading_zeros(S, T) :-
    atom_chars(S, Cs),
    drop_zeros(Cs, Clean),
    ( Clean = [] -> T = '0' ; atom_chars(T, Clean) ).

drop_zeros(['0'|T], R) :- drop_zeros(T, R).
drop_zeros(L, L).

pad_left_zeros(S, Padded) :-
    atom_length(S, Len),
    Missing is 8 - Len,
    Missing >= 0,
    length(Zeros, Missing),
    maplist(=('0'), Zeros),
    atom_chars(ZeroAtom, Zeros),
    atom_concat(ZeroAtom, S, Padded).

% Core rules
tell(_, 'read', permitted, ['1.1.1.1.1':permitted]).
tell(_, 'store', permitted, ['1.1.2.1.1':permitted]).
tell(_, 'validate:file:text/csv', permitted, ['1.1.2.2.1':permitted]).
tell(_, 'validate:file:application/json', permitted, ['1.1.2.2.2':permitted]).
tell(_, 'governance:change', permitted, ['1.1.3.1.1':permitted]).

% Dynamic rule with safety
tell(_, Action, permitted, ['1.1.2.3.1':permitted]) :-
    nonvar(Action),
    atom_concat('validate:file:size:', SizeAtomRaw, Action),
    trim_leading_zeros(SizeAtomRaw, SizeAtom),
    pad_left_zeros(SizeAtom, PaddedSize),
    max_file_size(Max),
    PaddedSize @=< Max.

% Default fallback
tell(_, _, prohibited, []).

tell_all(Who, Actions) :-
    bagof(Action:Result:Evidence, tell(Who, Action, Result, Evidence), Actions).

tell_permitted_actions(Who, PermittedActions) :-
    tell_all(Who, Actions),
    findall(PermittedAction, member(PermittedAction:permitted:_, Actions), PermittedActions).

EOF

# Check the generated file
PROGRAM=$(cat "$MINIO_GOV_PATH/$MINIO_WALLET-governance.pl")
$AXONED_PATH query logic ask \
  --node "$AXONE_NODE_RPC" \
  --program "$PROGRAM" \
  "tell(_, 'read', R, E)."

$AXONED_PATH query logic ask \
  --node "$AXONE_NODE_RPC" \
  --program "$PROGRAM" \
  "tell(_, 'validate:file:size:13522330', R, E)."

```

### Step 3: Submit Service Governance Code

Create a Law Stone contract instance for the service governance:

```bash
# Generate unique label
export MINIO_GOV_CODE_ID=$(uuidgen)
echo "MINIO_GOV_CODE_ID=\"$MINIO_GOV_CODE_ID\""
echo "MINIO_GOV_CODE_ID=\"$MINIO_GOV_CODE_ID\"" >> $LOG_PATH/implementation.log

# Submit program
export MINIO_GOV_CODE_TX_HASH=$($AXONED_PATH tx wasm instantiate $CODE_ID_LAW_STONE \
    "{\"program\":\"$(base64 -w 0 $MINIO_GOV_PATH/$MINIO_WALLET-governance.pl)\", \"storage_address\": \"$OBJECTARIUM_ADDR\"}" \
    --label $MINIO_GOV_CODE_ID \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --admin $ISSUER_ADDRESS \
    --gas 20000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$MINIO_GOV_CODE_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_GOV_CODE_TX_HASH=\"$MINIO_GOV_CODE_TX_HASH\"" 
echo "MINIO_GOV_CODE_TX_HASH=\"$MINIO_GOV_CODE_TX_HASH\"" >> $LOG_PATH/implementation.log

# Get contract address
export MINIO_GOV_CODE_ADDR=$($AXONED_PATH query tx $MINIO_GOV_CODE_TX_HASH --node $AXONE_NODE_RPC -o json | \
    jq -r '.events[] | select(.type == "instantiate") | .attributes[] | select(.key == "_contract_address") | .value')

echo "MINIO_GOV_CODE_ADDR=\"$MINIO_GOV_CODE_ADDR\""
echo "MINIO_GOV_CODE_ADDR=\"$MINIO_GOV_CODE_ADDR\"" >> $LOG_PATH/implementation.log
```

### Step 4: Create Service Governance Credentials

Create the governance credential file:

```bash
export MINIO_CRED_GOV_ID=$(uuidgen)
echo "MINIO_CRED_GOV_ID=\"$MINIO_CRED_GOV_ID\""
echo "MINIO_CRED_GOV_ID=\"$MINIO_CRED_GOV_ID\"" >> $LOG_PATH/implementation.log

cat <<EOF | envsubst > $MINIO_GOV_PATH/$MINIO_WALLET-governance-credential.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/governance/text/"
  ],
  "type": ["VerifiableCredential", "GovernanceTextCredential"],
  "id": "https://w3id.org/axone/ontology/$ONTOLOGY_MAYOR_VERSION/schema/credential/governance/text/$MINIO_CRED_GOV_ID",
  "credentialSubject": {
    "id": "$MINIO_DID",
    "isGovernedBy": {
      "type": "GovernanceText",
      "fromGovernance": "cosmwasm:law-stone:${MINIO_GOV_CODE_ADDR}?query=%22program_code%22"
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

### Step 5: Sign and Submit Service Governance Credentials

```bash
# Sign the credential
$AXONED_PATH credential sign $MINIO_GOV_PATH/$MINIO_WALLET-governance-credential.jsonld \
  $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $MINIO_GOV_PATH/$MINIO_WALLET-governance-credential.nq

# Encode inline and submit directly
export MINIO_CRED_GOV_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
  "{\"submit_claims\":{\
    \"claims\": \"$(base64 -w 0 $MINIO_GOV_PATH/$MINIO_WALLET-governance-credential.nq)\", \
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
wait_and_check_tx "$MINIO_CRED_GOV_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_CRED_GOV_TX_HASH=\"$MINIO_CRED_GOV_TX_HASH\"" 
echo "MINIO_CRED_GOV_TX_HASH=\"$MINIO_CRED_GOV_TX_HASH\"" >> $LOG_PATH/implementation.log
```

### Step 7: Testing Service Governance Code

```bash
# --------------------------
# Constants
# --------------------------

export TEST_DID="did:key:z16DtrNpHS569CKDUFaXGzAMnLdMoaanxHqqsPnd91ZnyufLL"
export LAW_STONE_ADDR=$MINIO_GOV_CODE_ADDR

# --------------------------
# Function: ask_query
# --------------------------
ask_query() {
  local QUERY="$1"
  local LABEL="$2"

  echo ""
  echo "🔍 $LABEL"
  local ENCODED=$(echo -n "{\"ask\":{\"query\":\"$QUERY\"}}" | base64 -w 0)

  local RESULT=$(grpcurl -plaintext -d @ \
    $AXONE_NODE_GRPC cosmwasm.wasm.v1.Query/SmartContractState <<EOF
{
  "address": "$LAW_STONE_ADDR",
  "query_data": "$ENCODED"
}
EOF
  )

  echo "$RESULT" | jq -r '.data' | base64 -d | jq
}
# --------------------------
# Test 1: tell_permitted_actions (overview)
# --------------------------

ask_query "tell_permitted_actions('$TEST_DID', Actions)." "Permitted actions overview"

# --------------------------
# Test 2: Specific actions
# --------------------------

ask_query "tell('$TEST_DID', 'store', Result, Evidence)." "Check permission: store"
ask_query "tell('$TEST_DID', 'read', Result, Evidence)." "Check permission: read"

# --------------------------
# Test 3: File type validations
# --------------------------

ask_query "tell('$TEST_DID', 'validate:file:text/csv', Result, Evidence)." "Validate type: text/csv"
ask_query "tell('$TEST_DID', 'validate:file:application/json', Result, Evidence)." "Validate type: application/json"
ask_query "tell('$TEST_DID', 'validate:file:application/pdf', Result, Evidence)." "Validate type: INVALID (application/pdf)"

# --------------------------
# Test 4: File size validations
# --------------------------

ask_query "tell('$TEST_DID', 'validate:file:size:52428800', Result, Evidence)." "Validate size: 50MB (valid)"
ask_query "tell('$TEST_DID', 'validate:file:size:123456', Result, Evidence)." "Validate size: 123 KB (valid)"
ask_query "tell('$TEST_DID', 'validate:file:size:99999999', Result, Evidence)." "Validate size: 99MB (INVALID)"

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

## Create MinIO S3 Storage Service Zone Membership

### Step 1: Create Service Zone Membership Credential

```bash
export MINIO_CRED_ZONE_MEMBERSHIP_ID=$(uuidgen)
echo "MINIO_CRED_ZONE_MEMBERSHIP_ID=\"$MINIO_CRED_ZONE_MEMBERSHIP_ID\""
echo "MINIO_CRED_ZONE_MEMBERSHIP_ID=\"$MINIO_CRED_ZONE_MEMBERSHIP_ID\"" >> $LOG_PATH/implementation.log

# Create service description
cat <<EOF | envsubst > $MINIO_DESC_PATH/$MINIO_WALLET-zone-membership-credential.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/$ONTOLOGY_NEXT_VERSION/schema/credential/zone/membership/"
  ],
   "type": ["VerifiableCredential", "ZoneMembershipCredential"],
  "id": "https://w3id.org/axone/ontology/$ONTOLOGY_NEXT_VERSION/schema/credential/zone/membership/$MINIO_CRED_DESCRIPTION_ID",
  "credentialSubject": {
    "id": "$MINIO_DID",
    "forResource": "$MINIO_DID",
    "inZone": "$ZONE_DID"
  },
  "issuanceDate": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "issuer": {
    "id": "$ISSUER_DID",
    "name": "$ISSUER_WALLET"
  }
}
EOF
```

### Step 4: Sign and Submit Service Zone Membership Credential

Sign and encode the service zone membership credentials:

```bash
$AXONED_PATH credential sign $MINIO_DESC_PATH/$MINIO_WALLET-zone-membership-credential.jsonld \
    $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $MINIO_DESC_PATH/$MINIO_WALLET-zone-membership-credential.nq

# Submit minio zone membership description to blockchain
export MINIO_CRED_ZONE_MEMBERSHIP_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
    "{\"submit_claims\":{\
        \"claims\": \"$(base64 -w 0 $MINIO_DESC_PATH/$MINIO_WALLET-zone-membership-credential.nq)\", \
        \"format\": \"n_quads\" \
    }}" \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --gas 10000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$MINIO_CRED_ZONE_MEMBERSHIP_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_CRED_ZONE_MEMBERSHIP_TX_HASH=\"$MINIO_CRED_ZONE_MEMBERSHIP_TX_HASH\"" 
echo "MINIO_CRED_ZONE_MEMBERSHIP_TX_HASH=\"$MINIO_CRED_ZONE_MEMBERSHIP_TX_HASH\"" >> $LOG_PATH/implementation.log
```

## Querying Cognitarium

### Get All Triples

```bash

QUERY=$(jq -n  '
{
  "select": 
  {
    "query": 
    {
      "prefixes": [],
      "select": [
          {
              "variable": "subject"
          },
          {
              "variable": "predicate"
          },
          {
              "variable": "object"
          }
      ],
      where: 
      {
        bgp: 
        {
          patterns: 
          [
            {
              subject: { variable: "subject" },
              predicate: { variable: "predicate" },
              object: { variable: "object" }
            }
          ]
        }
      }
    }
  }
}')

$AXONED_PATH query wasm contract-state smart $COGNITARIUM_ADDR "$QUERY" --node $AXONE_NODE_RPC --output json | jq

```

### Get MinIO Gov Address

```bash
# ------------------------------
# GET MINIO GOV CREDENTIAL FROM MINIO DID
# ------------------------------

QUERY=$(jq -n --arg did "$MINIO_DID" '{
  select: {
    query: {
      prefixes: [],
      select: [
        { variable: "credential" },
        { variable: "p" },
        { variable: "o" },
        { variable: "date" } 
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
                },
                {
                  subject: { variable: "credential" },
                  predicate: { named_node: { full: "dataverse:credential:body#validFrom" } },
                  object: { variable: "date" } 
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
```

### Get Zone DID from MinIO Service DID using Zone Membership Credential

This query retrieves the Zone DID associated with a given service (MinIO) DID based on the Zone Membership Credential.

```bash

# GET ZONE DID FROM MINIO DID

export QUERY=$(jq -n --arg minioDID "$MINIO_DID" '{
  select: {
    query: {
      prefixes: [],
      select: [{ variable: "zoneDID" }],
      where: {
        lateral_join: {
          left: {
            bgp: {
              patterns: [
                {
                  subject: { variable: "credential" },
                  predicate: { named_node: { full: "dataverse:credential:body#subject" } },
                  object: { node: { named_node: { full: $minioDID } } }
                },
                {
                  subject: { variable: "credential" },
                  predicate: { named_node: { full: "dataverse:credential:body#type" } },
                  object: { node: { named_node: { full: "https://w3id.org/axone/ontology/vnext/schema/credential/zone/membership/ZoneMembershipCredential" } } }
                },
                {
                  subject: { variable: "credential" },
                  predicate: { named_node: { full: "dataverse:credential:body#claim" } },
                  object: { variable: "claim" }
                }
              ]
            }
          },
          right: {
            bgp: {
              patterns: [
                {
                  subject: { variable: "claim" },
                  predicate: { named_node: { full: "https://w3id.org/axone/ontology/vnext/schema/credential/zone/membership/inZone" } },
                  object: { variable: "zoneDID" }
                }
              ]
            }
          }
        }
      }
    }
  }
}')

# Ejecutar consulta corregida
$AXONED_PATH query wasm contract-state smart $COGNITARIUM_ADDR "$QUERY" \
  --node $AXONE_NODE_RPC --output json | jq '.data.results.bindings'

```

### Get Credential From DID

```bash

DID="did:key:z6DtpnQQd9UxRMJTtqXA4JJvqP7rin3sC4QvJ4amsyayDuZA"

QUERY=$(jq -n --arg did "$DID" '{
  select: {
    query: {
      prefixes: [],
      select: [
        { "variable": "credential" }
      ],
      where: {
        bgp: {
          patterns: [
            {
              subject: { "variable": "credential" },
              predicate: {
                named_node: {
                  full: "dataverse:credential:body#subject"
                }
              },
              object: {
                node: {
                  named_node: {
                    full: $did
                  }
                }
              }
            }
          ]
        }
      }
    }
  }
}')

$AXONED_PATH query wasm contract-state smart $COGNITARIUM_ADDR "$QUERY" --node $AXONE_NODE_RPC --output json | jq

CRED_IRI="https://w3id.org/axone/ontology/v4/schema/credential/dataset/description/e1508ef0-fb4b-486e-b4a3-486d1fef79d5"

QUERY=$(jq -n --arg iri "$CRED_IRI" '{
  select: {
    query: {
      prefixes: [],
      select: [
        { "variable": "p" },
        { "variable": "o" }
      ],
      where: {
        bgp: {
          patterns: [
            {
              subject: {
                node: {
                  named_node: {
                    full: $iri
                  }
                }
              },
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

$AXONED_PATH query auth account axone1zyef5q2fqx9rgsnyfwc3w9ukfeyltl4hnn2ydg --node $AXONE_NODE_RPC 

echo "CpwBCokBChwvY29zbW9zLmJhbmsudjFiZXRhMS5Nc2dTZW5kEmkKLGF4b25lMXp5ZWY1cTJmcXg5cmdzbnlmd2Mzdzl1a2ZleWx0bDRobm4yeWRnEixheG9uZTF6eWVmNXEyZnF4OXJnc255ZndjM3c5dWtmZXlsdGw0aG5uMnlkZxoLCgZ1YXhvbmUSATESDlNlbGYtc2VuZCB0ZXN0EmgKUApGCh8vY29zbW9zLmNyeXB0by5zZWNwMjU2azEuUHViS2V5EiMKIQKowPaYaKTolLjYB7JzL6EWBPI2cXBNKM61qDa8hKNseRIECgIIARhfEhQKDgoGdWF4b25lEgQyNTAwEKCNBhpYBwAAAAAHAAAAAAAACQAEAAADAAAAAAAAAAAABwAEAAAABwAAAAAAAAAACAAAAAcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACCAAAAAAGAAAAAAAAAAAAAA==" | base64 -d > tx.raw
echo $AXONED_PATH  tx broadcast tx.raw  --node $AXONE_NODE_RPC --chain-id $NETWORK --broadcast-mode sync

$AXONED_PATH  tx broadcast tx.raw  --node $AXONE_NODE_RPC --chain-id $NETWORK --broadcast-mode sync


curl -X POST https://api.dentrite.axone.xyz:443/rpc/broadcast_tx_sync \
  -H "Content-Type: application/json" \
  -d '{
    "tx_bytes": "CpAoCs4nCiQvY29zbXdhc20ud2FzbS52MS5Nc2dFeGVjdXRlQ29udHJhY3QSpScKLGF4b25lMXp5ZWY1cTJmcXg5cmdzbnlmd2Mzdzl1a2ZleWx0bDRobm4yeWRnEkBheG9uZTFubTN5a3R6Y2dwbnZ3dTZxcHpxZ2wya3R5dmxnc3N0Yzdldjg0OWRkM3VsYXlndzc1bXFxNnNhYXJ3GrImeyJzdWJtaXRfY2xhaW1zIjp7ImNsYWltcyI6IlBHUnBaRHByWlhrNmVqWkVkSEJRUm0xUU16aFFOMEV5V0ZwSVFsZ3hialZIYURkYU1sWk1TRE53UjBWRGVqZFVZbTlITjFjMVFWVnlQaUE4YUhSMGNITTZMeTkzTTJsa0xtOXlaeTloZUc5dVpTOXZiblJ2Ykc5bmVTOTJOQzl6WTJobGJXRXZZM0psWkdWdWRHbGhiQzlrWVhSaGMyVjBMMlJsYzJOeWFYQjBhVzl1TDJoaGMwUmxjMk55YVhCMGFXOXVQaUFpVFdWa2FXTmhiQ0JrWVhSaGMyVjBJSFZ3Ykc5aFpHVmtJSFJ2SUVSdlkzUkJTU0J3YkdGMFptOXliUzRnUm1sc1pUb2daWFJsY201c0xYQnlaWEJ5YjJRdGVIQjFZakZzYTNJeU5HVnNaelZvT1Mxd2JTNXFjMjl1TENCVGFYcGxPaUF4TGpjNElFdENJaUF1Q2p4a2FXUTZhMlY1T25vMlJIUndVRVp0VURNNFVEZEJNbGhhU0VKWU1XNDFSMmczV2pKV1RFZ3pjRWRGUTNvM1ZHSnZSemRYTlVGVmNqNGdQR2gwZEhCek9pOHZkek5wWkM1dmNtY3ZZWGh2Ym1VdmIyNTBiMnh2WjNrdmRqUXZjMk5vWlcxaEwyTnlaV1JsYm5ScFlXd3ZaR0YwWVhObGRDOWtaWE5qY21sd2RHbHZiaTlvWVhOR2IzSnRZWFErSUR4b2RIUndjem92TDNjemFXUXViM0puTDJGNGIyNWxMMjl1ZEc5c2IyZDVMM1kwTDNSb1pYTmhkWEoxY3k5dFpXUnBZUzEwZVhCbEwyRndjR3hwWTJGMGFXOXVYMnB6YjI0K0lDNEtQR1JwWkRwclpYazZlalpFZEhCUVJtMVFNemhRTjBFeVdGcElRbGd4YmpWSGFEZGFNbFpNU0ROd1IwVkRlamRVWW05SE4xYzFRVlZ5UGlBOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5aGVHOXVaUzl2Ym5SdmJHOW5lUzkyTkM5elkyaGxiV0V2WTNKbFpHVnVkR2xoYkM5a1lYUmhjMlYwTDJSbGMyTnlhWEIwYVc5dUwyaGhjMGRsYjBOdmRtVnlZV2RsUGlBOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5aGVHOXVaUzl2Ym5SdmJHOW5lUzkyTkM5MGFHVnpZWFZ5ZFhNdllYSmxZUzFqYjJSbEx6ZzBNRDRnTGdvOFpHbGtPbXRsZVRwNk5rUjBjRkJHYlZBek9GQTNRVEpZV2toQ1dERnVOVWRvTjFveVZreElNM0JIUlVONk4xUmliMGMzVnpWQlZYSStJRHhvZEhSd2N6b3ZMM2N6YVdRdWIzSm5MMkY0YjI1bEwyOXVkRzlzYjJkNUwzWTBMM05qYUdWdFlTOWpjbVZrWlc1MGFXRnNMMlJoZEdGelpYUXZaR1Z6WTNKcGNIUnBiMjR2YUdGelZHRm5QaUFpUkdGMFlYTmxkQ0lnTGdvOFpHbGtPbXRsZVRwNk5rUjBjRkJHYlZBek9GQTNRVEpZV2toQ1dERnVOVWRvTjFveVZreElNM0JIUlVONk4xUmliMGMzVnpWQlZYSStJRHhvZEhSd2N6b3ZMM2N6YVdRdWIzSm5MMkY0YjI1bEwyOXVkRzlzYjJkNUwzWTBMM05qYUdWdFlTOWpjbVZrWlc1MGFXRnNMMlJoZEdGelpYUXZaR1Z6WTNKcGNIUnBiMjR2YUdGelZHRm5QaUFpUkc5amRFRkpJaUF1Q2p4a2FXUTZhMlY1T25vMlJIUndVRVp0VURNNFVEZEJNbGhhU0VKWU1XNDFSMmczV2pKV1RFZ3pjRWRGUTNvM1ZHSnZSemRYTlVGVmNqNGdQR2gwZEhCek9pOHZkek5wWkM1dmNtY3ZZWGh2Ym1VdmIyNTBiMnh2WjNrdmRqUXZjMk5vWlcxaEwyTnlaV1JsYm5ScFlXd3ZaR0YwWVhObGRDOWtaWE5qY21sd2RHbHZiaTlvWVhOVVpXMXdiM0poYkVOdmRtVnlZV2RsUGlBOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5aGVHOXVaUzl2Ym5SdmJHOW5lUzkyTkM5MGFHVnpZWFZ5ZFhNdmRHVnRjRzl5WVd3dGNtRnVaMlV2TWpBeU5GOHdNVjh3TVY4eU1ESTJYekV5WHpNeFBpQXVDanhrYVdRNmEyVjVPbm8yUkhSd1VFWnRVRE00VURkQk1saGFTRUpZTVc0MVIyZzNXakpXVEVnemNFZEZRM28zVkdKdlJ6ZFhOVUZWY2o0Z1BHaDBkSEJ6T2k4dmR6TnBaQzV2Y21jdllYaHZibVV2YjI1MGIyeHZaM2t2ZGpRdmMyTm9aVzFoTDJOeVpXUmxiblJwWVd3dlpHRjBZWE5sZEM5a1pYTmpjbWx3ZEdsdmJpOW9ZWE5VYVhSc1pUNGdJbVYwWlhKdWJDMXdjbVZ3Y205a0xYaHdkV0l4Ykd0eU1qUmxiR2MxYURrdGNHMHVhbk52YmlBdElFUnZZM1JCU1NCRVlYUmhjMlYwSWlBdUNqeGthV1E2YTJWNU9ubzJSSFJ3VUVadFVETTRVRGRCTWxoYVNFSllNVzQxUjJnM1dqSldURWd6Y0VkRlEzbzNWR0p2UnpkWE5VRlZjajRnUEdoMGRIQnpPaTh2ZHpOcFpDNXZjbWN2WVhodmJtVXZiMjUwYjJ4dloza3ZkalF2YzJOb1pXMWhMMk55WldSbGJuUnBZV3d2WkdGMFlYTmxkQzlrWlhOamNtbHdkR2x2Ymk5b1lYTlViM0JwWXo0Z1BHaDBkSEJ6T2k4dmR6TnBaQzV2Y21jdllYaHZibVV2YjI1MGIyeHZaM2t2ZGpRdmRHaGxjMkYxY25WekwzUnZjR2xqTDJobFlXeDBhRDRnTGdvOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5aGVHOXVaUzl2Ym5SdmJHOW5lUzkyTkM5elkyaGxiV0V2WTNKbFpHVnVkR2xoYkM5a1lYUmhjMlYwTDJSbGMyTnlhWEIwYVc5dUx6ZzJNV1JoT1dRM0xUaGxORGN0TkdJMlpTMDVaalF3TFdZeE1UZG1abVEyTTJWbFlUNGdQR2gwZEhBNkx5OTNkM2N1ZHpNdWIzSm5MekU1T1Rrdk1ESXZNakl0Y21SbUxYTjViblJoZUMxdWN5TjBlWEJsUGlBOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5aGVHOXVaUzl2Ym5SdmJHOW5lUzkyTkM5elkyaGxiV0V2WTNKbFpHVnVkR2xoYkM5a1lYUmhjMlYwTDJSbGMyTnlhWEIwYVc5dUwwUmhkR0Z6WlhSRVpYTmpjbWx3ZEdsdmJrTnlaV1JsYm5ScFlXdytJQzRLUEdoMGRIQnpPaTh2ZHpOcFpDNXZjbWN2WVhodmJtVXZiMjUwYjJ4dloza3ZkalF2YzJOb1pXMWhMMk55WldSbGJuUnBZV3d2WkdGMFlYTmxkQzlrWlhOamNtbHdkR2x2Ymk4NE5qRmtZVGxrTnkwNFpUUTNMVFJpTm1VdE9XWTBNQzFtTVRFM1ptWmtOak5sWldFK0lEeG9kSFJ3T2k4dmQzZDNMbmN6TG05eVp5OHhPVGs1THpBeUx6SXlMWEprWmkxemVXNTBZWGd0Ym5NamRIbHdaVDRnUEdoMGRIQnpPaTh2ZDNkM0xuY3pMbTl5Wnk4eU1ERTRMMk55WldSbGJuUnBZV3h6STFabGNtbG1hV0ZpYkdWRGNtVmtaVzUwYVdGc1BpQXVDanhvZEhSd2N6b3ZMM2N6YVdRdWIzSm5MMkY0YjI1bEwyOXVkRzlzYjJkNUwzWTBMM05qYUdWdFlTOWpjbVZrWlc1MGFXRnNMMlJoZEdGelpYUXZaR1Z6WTNKcGNIUnBiMjR2T0RZeFpHRTVaRGN0T0dVME55MDBZalpsTFRsbU5EQXRaakV4TjJabVpEWXpaV1ZoUGlBOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5elpXTjFjbWwwZVNOd2NtOXZaajRnWHpwaU1DQXVDanhvZEhSd2N6b3ZMM2N6YVdRdWIzSm5MMkY0YjI1bEwyOXVkRzlzYjJkNUwzWTBMM05qYUdWdFlTOWpjbVZrWlc1MGFXRnNMMlJoZEdGelpYUXZaR1Z6WTNKcGNIUnBiMjR2T0RZeFpHRTVaRGN0T0dVME55MDBZalpsTFRsbU5EQXRaakV4TjJabVpEWXpaV1ZoUGlBOGFIUjBjSE02THk5M2QzY3Vkek11YjNKbkx6SXdNVGd2WTNKbFpHVnVkR2xoYkhNalkzSmxaR1Z1ZEdsaGJGTjFZbXBsWTNRK0lEeGthV1E2YTJWNU9ubzJSSFJ3VUVadFVETTRVRGRCTWxoYVNFSllNVzQxUjJnM1dqSldURWd6Y0VkRlEzbzNWR0p2UnpkWE5VRlZjajRnTGdvOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5aGVHOXVaUzl2Ym5SdmJHOW5lUzkyTkM5elkyaGxiV0V2WTNKbFpHVnVkR2xoYkM5a1lYUmhjMlYwTDJSbGMyTnlhWEIwYVc5dUx6ZzJNV1JoT1dRM0xUaGxORGN0TkdJMlpTMDVaalF3TFdZeE1UZG1abVEyTTJWbFlUNGdQR2gwZEhCek9pOHZkM2QzTG5jekxtOXlaeTh5TURFNEwyTnlaV1JsYm5ScFlXeHpJMmx6YzNWaGJtTmxSR0YwWlQ0Z0lqSXdNalV0TURVdE1qRlVNRFk2TVRNNk1Ea3VPRGMzV2lKZVhqeG9kSFJ3T2k4dmQzZDNMbmN6TG05eVp5OHlNREF4TDFoTlRGTmphR1Z0WVNOa1lYUmxWR2x0WlQ0Z0xnbzhhSFIwY0hNNkx5OTNNMmxrTG05eVp5OWhlRzl1WlM5dmJuUnZiRzluZVM5Mk5DOXpZMmhsYldFdlkzSmxaR1Z1ZEdsaGJDOWtZWFJoYzJWMEwyUmxjMk55YVhCMGFXOXVMemcyTVdSaE9XUTNMVGhsTkRjdE5HSTJaUzA1WmpRd0xXWXhNVGRtWm1RMk0yVmxZVDRnUEdoMGRIQnpPaTh2ZDNkM0xuY3pMbTl5Wnk4eU1ERTRMMk55WldSbGJuUnBZV3h6STJsemMzVmxjajRnUEdScFpEcHJaWGs2ZWpaTmEzUTFaVU51YlhOcE1rMUVkamxNTjBWamRrMVpOM1ZHUTNvM1FWVklZM1JoV1VkbWVFTmFjVEl6VEV4bFBpQXVDbDg2WWpFZ1BHaDBkSEE2THk5d2RYSnNMbTl5Wnk5a1l5OTBaWEp0Y3k5amNtVmhkR1ZrUGlBaU1qQXlOUzB3TlMweU1WUXdOam94TXpvd09Wb2lYbDQ4YUhSMGNEb3ZMM2QzZHk1M015NXZjbWN2TWpBd01TOVlUVXhUWTJobGJXRWpaR0YwWlZScGJXVStJRjg2WWpBZ0xncGZPbUl4SUR4b2RIUndPaTh2ZDNkM0xuY3pMbTl5Wnk4eE9UazVMekF5THpJeUxYSmtaaTF6ZVc1MFlYZ3Ribk1qZEhsd1pUNGdQR2gwZEhCek9pOHZkek5wWkM1dmNtY3ZjMlZqZFhKcGRIa2pSV1F5TlRVeE9WTnBaMjVoZEhWeVpUSXdNakErSUY4NllqQWdMZ3BmT21JeElEeG9kSFJ3Y3pvdkwzY3phV1F1YjNKbkwzTmxZM1Z5YVhSNUkzQnliMjltVUhWeWNHOXpaVDRnUEdoMGRIQnpPaTh2ZHpOcFpDNXZjbWN2YzJWamRYSnBkSGtqWVhOelpYSjBhVzl1VFdWMGFHOWtQaUJmT21Jd0lDNEtYenBpTVNBOGFIUjBjSE02THk5M00ybGtMbTl5Wnk5elpXTjFjbWwwZVNOd2NtOXZabFpoYkhWbFBpQWllalIyYVRKWmRHOUZkVFJyY201aWNWRjJWbkIwVmtwMFJrZGhlbWRDUW5jelpVZElURVJvZVRSUmMyVTVZa3BFZUdGV2RWSmxUV1V4V2toRmFHdEdOVEk0YzFOdldsWmFkVmRrU25OQ1dqWlhNMFZsYlhOb1pVZ2lYbDQ4YUhSMGNITTZMeTkzTTJsa0xtOXlaeTl6WldOMWNtbDBlU050ZFd4MGFXSmhjMlUrSUY4NllqQWdMZ3BmT21JeElEeG9kSFJ3Y3pvdkwzY3phV1F1YjNKbkwzTmxZM1Z5YVhSNUkzWmxjbWxtYVdOaGRHbHZiazFsZEdodlpENGdQR1JwWkRwclpYazZlalpOYTNRMVpVTnViWE5wTWsxRWRqbE1OMFZqZGsxWk4zVkdRM28zUVZWSVkzUmhXVWRtZUVOYWNUSXpURXhsSTNvMlRXdDBOV1ZEYm0xemFUSk5SSFk1VERkRlkzWk5XVGQxUmtONk4wRlZTR04wWVZsSFpuaERXbkV5TTB4TVpUNGdYenBpTUNBdUNnPT0iLCJmb3JtYXQiOiJuX3F1YWRzIn19Ej1VcGxvYWQgY3JlZGVudGlhbCBmb3IgZXRlcm5sLXByZXByb2QteHB1YjFsa3IyNGVsZzVoOS1wbS5qc29uEmgKUApGCh8vY29zbW9zLmNyeXB0by5zZWNwMjU2azEuUHViS2V5EiMKIQKowPaYaKTolLjYB7JzL6EWBPI2cXBNKM61qDa8hKNseRIECgIIARhfEhQKDgoGdWF4b25lEgQyNTAwEKCNBhpYAAAAAAUJAQAACQAAAAgAAAAAAAACAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAGAAAAAAAAAAAAAAAFAAACAAMCAAAGAAABAAAAAAAAAAAAAAAAAAAAAAAAAA==",
    "mode": "BROADCAST_MODE_SYNC"
  }'


```
