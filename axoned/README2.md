
# README MINIO STORAGE SERVICE

## Table of Contents

- [README MINIO STORAGE SERVICE](#readme-minio-storage-service)
  - [Table of Contents](#table-of-contents)
  - [Create Minio S3 Storage Service](#create-minio-s3-storage-service)
    - [Step 1: Set Env for Minio DID Description](#step-1-set-env-for-minio-did-description)
    - [Step 2: Key and DID Creation](#step-2-key-and-did-creation)
    - [Step 3: Create Credentials](#step-3-create-credentials)
    - [Step 4: Sign Credentials](#step-4-sign-credentials)
    - [Step 5: Register in Blockchain](#step-5-register-in-blockchain)
  - [Create Minio Governance](#create-minio-governance)
    - [Step 1: Set Env for Zone DID Governance](#step-1-set-env-for-zone-did-governance)
    - [Step 2: Create Governance Rules](#step-2-create-governance-rules)
    - [Step 3: Submit Prolog Program to Blockchain](#step-3-submit-prolog-program-to-blockchain)
    - [Step 4: Create Governance Credentials](#step-4-create-governance-credentials)
    - [Step 5: Sign and Submit Governance Credentials](#step-5-sign-and-submit-governance-credentials)
    - [Step 7: Testing](#step-7-testing)
  - [Create Minio S3 Zone Membership Credential](#create-minio-s3-zone-membership-credential)
    - [Step 1: Create Zone Membership Credential](#step-1-create-zone-membership-credential)
    - [Step 4: Sign Credentials](#step-4-sign-credentials-1)
    - [Step 5: Register in Blockchain](#step-5-register-in-blockchain-1)

## Create Minio S3 Storage Service

### Step 1: Set Env for Minio DID Description

```bash
WORK_DIR_AXONE=/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned
FILES_DIR=/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/files
LOG_PATH=/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/logs
AXONED_PATH=/home/manuelpadilla/sources/reposUbuntu/AXONE/tools/axoned-10.0.0-linux-amd64/axoned
MAYOR=4
AXONE_NODE_RPC=https://api.dentrite.axone.xyz:443/rpc
AXONE_NODE_GRPC=axone-testnet-grpc.polkachu.com:17690
NETWORK=axone-dentrite-1
KEYRING_BACKEND="--keyring-backend os"
CODE_ID_DATAVERSE=4
CODE_ID_COGNITARIUM=3
CODE_ID_LAW_STONE=2
CODE_ID_OBJECTARIUM=1
ISSUER_WALLET=doctai-issuer-wallet
ISSUER_ADDRESS=axone1l3tjnxllfmy4p3082vd388jp8j9m72ypeynm9y
ISSUER_DID=did:key:zQ3shN45CkRnjackWYwM191q2bNanAYeP1ZvxoAEPaV4ZpgZo
ISSUER_ACCOUNT_NUMBER=47013
ISSUER_SEQUENCE=11
OBJECTARIUM_LABEL=my-objectarium
OBJECTARIUM_TX_HASH=3C2D4C3819C8F59E872835DD77EA3CE39A21105CC156867FB7307FE7B5B3F921
OBJECTARIUM_ADDR=axone1fchxlkly99gqymnvpfdyqjw9xc2cx62yplvw3rcfl2t5wurj3j9sa7pn8y
DATAVERSE_LABEL=my-dataverse
DATAVERSE_TX_HASH=A45E32448685F79B89268944F114C238080A11ED72D00F0516A9503AF8393C27
DATAVERSE_ADDR=axone19cq5ladm6mxu8clszem8d8cwj9haac4du08s5zq2uvpyg4ue625qqzm29g
COGNITARIUM_ADDR=axone1dg5acs48rj4gezvruawvcy4y7yc4frt2v4an0dzux2dqcsq3ju0sqm62mw
ZONE_PATH=/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/files/zones/description
ZONE_WALLET=hearth-labs-wallet
ZONE_ADDRESS=axone1ukpvhk9jmgmqktmcph23eyr0q3pwae0tl86sqz
ZONE_DID=did:key:zQ3shmDoRRiFmNzwDrnV2iPdgA9xaoAYsSq8GC9tGHsNL4R7a
ZONE_DESCRIPTION_ID=d99cfd2e-d2ae-43af-9d38-ed16ceeb86ac
ZONE_DESCRIPTION_TX_HASH=88E9AF3956CF2CA07343D2A8F45A4C34A8BFF7008D63393CF09C77A2B19CA807
ZONE_GOV_PATH=/home/manuelpadilla/sources/reposUbuntu/AXONE/doctai/axoned/files/zones/governance
ZONE_GOV_ID=9f96a2c3-c679-4dbb-ae68-8f7026ef4c3f
ZONE_GOV_SUBMIT_PROGRAM_TX_HASH=6FF46B884AD2A662BB9537369104EC1FE3568E3A25C952E357B1E8CB91B82641
ZONE_GOV_ADDR=axone1c2a2l6swj7mcu6gp3v70a58m9sn6x5wsuh4ukr2tnra7hnnst3aqy07vye
ZONE_GOV_CRED_ID=c9648166-bf11-43c6-8825-1ff26634b3cd
ZONE_GOV_CRED_TX_HASH=B903A62180218794348EE324EB1BF099A6C775FEFC1401204DF31B1B011FF47D

# Load helper functions
source $WORK_DIR_AXONE/scripts/helpers.sh

```

```bash
export MINIO_PATH=$WORK_DIR_AXONE/files/minio/description
mkdir -p $MINIO_PATH

export MINIO_WALLET=minio-wallet

echo "MINIO_PATH: $MINIO_PATH"
echo "MINIO_PATH: $MINIO_PATH" >> $LOG_PATH/implementation.log
echo "MINIO_WALLET: $MINIO_WALLET"
echo "MINIO_WALLET: $MINIO_WALLET" >> $LOG_PATH/implementation.log

```

### Step 2: Key and DID Creation

```bash
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
# Get minio did o DID
export MINIO_ADDRESS=$($AXONED_PATH keys show $MINIO_WALLET -a $KEYRING_BACKEND)
export MINIO_DID=$($AXONED_PATH keys show $MINIO_WALLET -k $KEYRING_BACKEND)

export MINIO_PHRASE="actress outer equal gain brisk ship dawn patch sock culture junior wise banner train panic demise nothing eye tortoise north vibrant people gym shed"

echo "MINIO_ADDRESS: $MINIO_ADDRESS"
echo "MINIO_ADDRESS: $MINIO_ADDRESS" >> $LOG_PATH/implementation.log
echo "MINIO_DID: $MINIO_DID"
echo "MINIO_DID: $MINIO_DID" >> $LOG_PATH/implementation.log
echo "MINIO_PHRASE: $MINIO_PHRASE"
echo "MINIO_PHRASE: $MINIO_PHRASE" >> $LOG_PATH/implementation.log
```

### Step 3: Create Credentials

```bash
export MINIO_DESCRIPTION_ID=$(uuidgen)
echo "MINIO_DESCRIPTION_ID: $MINIO_DESCRIPTION_ID"
echo "MINIO_DESCRIPTION_ID: $MINIO_DESCRIPTION_ID" >> $LOG_PATH/implementation.log

# Create service description
cat <<EOF | envsubst > $MINIO_PATH/$MINIO_WALLET-description.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/v$MAYOR/schema/credential/digital-service/description/"
  ],
  "type": ["VerifiableCredential", "DigitalServiceDescriptionCredential"],
  "id": "https://w3id.org/axone/ontology/v$MAYOR/schema/credential/digital-service/description/$MINIO_DESCRIPTION_ID",
  "credentialSubject": {
    "id": "$MINIO_DID",
    "hasCategory": "https://w3id.org/axone/ontology/v$MAYOR/thesaurus/digital-service-category/Storage",
    "hasDescription": "Minio S3",
    "hasPublisher": "Me",
    "hasTag": [
      "Storage",
      "Cloud"
    ],
    "hasTitle": "My Minio"
  },
  "issuanceDate": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "issuer": {
    "id": "$ISSUER_DID",
    "name": "$ISSUER_WALLET"
  }
}
EOF
```

### Step 4: Sign Credentials

Sign and encode the zone credentials:

```bash
$AXONED_PATH credential sign $MINIO_PATH/$MINIO_WALLET-description.jsonld \
    $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $MINIO_PATH/$MINIO_WALLET-description.nq
```

### Step 5: Register in Blockchain

```bash
# Submit minio description to blockchain
export MINIO_DESCRIPTION_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
    "{\"submit_claims\":{\
        \"claims\": \"$(base64 -w 0 $MINIO_PATH/$MINIO_WALLET-description.nq)\", \
        \"format\": \"n_quads\" \
    }}" \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --gas 10000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$MINIO_DESCRIPTION_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_DESCRIPTION_TX_HASH: $MINIO_DESCRIPTION_TX_HASH" 
echo "MINIO_DESCRIPTION_TX_HASH: $MINIO_DESCRIPTION_TX_HASH" >> $LOG_PATH/implementation.log
```

## Create Minio Governance

### Step 1: Set Env for Zone DID Governance

```bash
export MINIO_GOV_PATH=$WORK_DIR_AXONE/files/minio/governance
mkdir -p $MINIO_GOV_PATH

echo "MINIO_GOV_PATH: $MINIO_GOV_PATH"
echo "MINIO_GOV_PATH: $MINIO_GOV_PATH" >> $LOG_PATH/implementation.log
```

### Step 2: Create Governance Rules

```bash
# Create service governance Prolog rules
cat <<EOF | envsubst > $MINIO_GOV_PATH/$MINIO_WALLET-governance.pl
% Minio S3 Governance
% ===========================

:- discontiguous([title/2, partOf/2, chapter/1, section/1, subSection/1, article/1, paragraph/4]).

% Main structure
title('1', 'MinIO Service').
chapter('1').

    title('1.1', 'Service Access Control'). partOf('1.1', '1').
    section('1.1').

        title('1.1.1', 'Public Access'). partOf('1.1.1', '1.1').
        subSection('1.1.1').

            title('1.1.1.1', 'Use'). partOf('1.1.1.1', '1.1.1').
            article('1.1.1.1').

                title('1.1.1.1.1', 'Use is allowed for everyone'). partOf('1.1.1.1.1', '1.1.1.1').
                paragraph('1.1.1.1.1', permitted, _, 'use').

        title('1.1.2', 'Restricted Access'). partOf('1.1.2', '1.1').
        subSection('1.1.2').

            title('1.1.2.1', 'Store'). partOf('1.1.2.1', '1.1.2').
            article('1.1.2.1').

                title('1.1.2.1.1', 'Store is allowed for everyone'). partOf('1.1.2.1.1', '1.1.2.1').
                paragraph('1.1.2.1.1', permitted, _, 'store').

            title('1.1.2.2', 'Read'). partOf('1.1.2.2', '1.1.2').
            article('1.1.2.2').

                title('1.1.2.2.1', 'Read is allowed for everyone'). partOf('1.1.2.2.1', '1.1.2.2').
                paragraph('1.1.2.2.1', permitted, _, 'read').

        title('1.1.3', 'Governance'). partOf('1.1.3', '1.1').
        subSection('1.1.3').

            title('1.1.3.1', 'Change'). partOf('1.1.3.1', '1.1.3').
            article('1.1.3.1').

                title('1.1.3.1.1', 'Governance change is allowed for everyone'). partOf('1.1.3.1.1', '1.1.3.1').
                paragraph('1.1.3.1.1', permitted, _, 'governance:change').

% Action resolution
resolved_action_context(Who, _, Context) :- Context = Who.

% SDK required predicates
tell(Who, Action, Result, Evidence) :-
    resolved_action_context(Who, Action, Context),
    bagof(P:Modality, paragraph(P, Modality, Context, Action), Evidence),
    (   member(_: permitted, Evidence) -> Result = permitted
    ;   Result = prohibited
    ).

tell_all(Who, Actions) :-
    bagof(Action:Result:Evidence, tell(Who, Action, Result, Evidence), Actions).

tell_permitted_actions(Who, PermittedActions) :-
    tell_all(Who, Actions),
    findall(PermittedAction, member(PermittedAction:permitted:_, Actions), PermittedActions).
EOF
```

### Step 3: Submit Prolog Program to Blockchain

Create a Law Stone contract instance for the zone governance:

```bash
# Generate unique label
export MINIO_GOV_ID=$(uuidgen)
echo "MINIO_GOV_ID: $MINIO_GOV_ID"
echo "MINIO_GOV_ID: $MINIO_GOV_ID" >> $LOG_PATH/implementation.log

# Submit program
export MINIO_GOV_SUBMIT_PROGRAM_TX_HASH=$($AXONED_PATH tx wasm instantiate $CODE_ID_LAW_STONE \
    "{\"program\":\"$(base64 -w 0 $MINIO_GOV_PATH/$MINIO_WALLET-governance.pl)\", \"storage_address\": \"$OBJECTARIUM_ADDR\"}" \
    --label $MINIO_GOV_ID \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --admin $ISSUER_ADDRESS \
    --gas 20000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$MINIO_GOV_SUBMIT_PROGRAM_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_GOV_SUBMIT_PROGRAM_TX_HASH: $MINIO_GOV_SUBMIT_PROGRAM_TX_HASH" 
echo "MINIO_GOV_SUBMIT_PROGRAM_TX_HASH: $MINIO_GOV_SUBMIT_PROGRAM_TX_HASH" >> $LOG_PATH/implementation.log

# Get contract address
export MINIO_GOV_ADDR=$($AXONED_PATH query tx $MINIO_GOV_SUBMIT_PROGRAM_TX_HASH --node $AXONE_NODE_RPC -o json | \
    jq -r '.events[] | select(.type == "instantiate") | .attributes[] | select(.key == "_contract_address") | .value')

echo "MINIO_GOV_ADDR: $MINIO_GOV_ADDR"
echo "MINIO_GOV_ADDR: $MINIO_GOV_ADDR" >> $LOG_PATH/implementation.log
```

### Step 4: Create Governance Credentials

Create the governance credential file:

```bash
export MINIO_GOV_CRED_ID=$(uuidgen)
echo "MINIO_GOV_CRED_ID: $MINIO_GOV_CRED_ID"
echo "MINIO_GOV_CRED_ID: $MINIO_GOV_CRED_ID" >> $LOG_PATH/implementation.log

cat <<EOF | envsubst > $MINIO_GOV_PATH/$MINIO_WALLET-governance-credential.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/v$MAYOR/schema/credential/governance/text/"
  ],
  "type": ["VerifiableCredential", "GovernanceTextCredential"],
  "id": "https://w3id.org/axone/ontology/v$MAYOR/schema/credential/governance/text/$MINIO_GOV_CRED_ID",
  "credentialSubject": {
    "id": "$MINIO_DID",
    "isGovernedBy": {
      "type": "GovernanceText",
      "fromGovernance": "cosmwasm:law-stone:${MINIO_GOV_ADDR}?query=%22program_code%22"
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

### Step 5: Sign and Submit Governance Credentials

```bash
# Sign the credential
$AXONED_PATH credential sign $MINIO_GOV_PATH/$MINIO_WALLET-governance-credential.jsonld \
  $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $MINIO_GOV_PATH/$MINIO_WALLET-governance-credential.nq

# Encode inline and submit directly
export MINIO_GOV_CRED_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
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
wait_and_check_tx "$MINIO_GOV_CRED_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_GOV_CRED_TX_HASH: $MINIO_GOV_CRED_TX_HASH" 
echo "MINIO_GOV_CRED_TX_HASH: $MINIO_GOV_CRED_TX_HASH" >> $LOG_PATH/implementation.log
```

### Step 7: Testing

```bash
# Identidad a testear
export TEST_DID="did:key:z16DtrNpHS569CKDUFaXGzAMnLdMoaanxHqqsPnd91ZnyufLL"
export TEST_ACTION="store"     # acción a verificar


# Contrato law-stone
export LAW_STONE_ADDR=$MINIO_GOV_ADDR

# ------------------------------
# ASK via gRPC (tell_permitted_actions)
# ------------------------------

export PROLOG_QUERY="tell_permitted_actions('$TEST_DID', Actions)."
export ENCODED_QUERY=$(echo -n "{\"ask\":{\"query\":\"$PROLOG_QUERY\"}}" | base64 -w 0)

echo "🔍 GRPC tell_permitted_actions for DID: $TEST_DID"

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
## Create Minio S3 Zone Membership Credential   

### Step 1: Create Zone Membership Credential

```bash
export MINIO_ZONE_CREDENTIAL_ID=$(uuidgen)
echo "MINIO_ZONE_CREDENTIAL_ID: $MINIO_ZONE_CREDENTIAL_ID"
echo "MINIO_ZONE_CREDENTIAL_ID: $MINIO_ZONE_CREDENTIAL_ID" >> $LOG_PATH/implementation.log

# Create service description
cat <<EOF | envsubst > $MINIO_PATH/$MINIO_WALLET-zone-membership-credential.jsonld
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://w3id.org/axone/ontology/v$MAYOR/schema/credential/zone/membership/"
  ],
   "type": ["VerifiableCredential", "ZoneMembershipCredential"],
  "id": "https://w3id.org/axone/ontology/v4/schema/credential/zone/membership/$MINIO_DESCRIPTION_ID",
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

### Step 4: Sign Credentials

Sign and encode the zone credentials:

```bash
$AXONED_PATH credential sign $MINIO_PATH/$MINIO_WALLET-zone-membership-credential.jsonld \
    $KEYRING_BACKEND --from $ISSUER_WALLET | jsonld toRdf -q - > $MINIO_PATH/$MINIO_WALLET-zone-membership-credential.nq
```

### Step 5: Register in Blockchain

```bash
# Submit minio description to blockchain
export MINIO_ZONE_CREDENTIAL_TX_HASH=$($AXONED_PATH tx wasm execute $DATAVERSE_ADDR \
    "{\"submit_claims\":{\
        \"claims\": \"$(base64 -w 0 $MINIO_PATH/$MINIO_WALLET-description.nq)\", \
        \"format\": \"n_quads\" \
    }}" \
    --node $AXONE_NODE_RPC \
    --chain-id $NETWORK \
    $KEYRING_BACKEND --from $ISSUER_ADDRESS \
    --account-number $ISSUER_ACCOUNT_NUMBER \
    --sequence $ISSUER_SEQUENCE \
    --gas 10000000 \
    --yes -o json | jq -r '.txhash')

wait_and_check_tx "$MINIO_ZONE_CREDENTIAL_TX_HASH" "$AXONE_NODE_RPC" "$AXONED_PATH"

echo "MINIO_ZONE_CREDENTIAL_TX_HASH: $MINIO_ZONE_CREDENTIAL_TX_HASH" 
echo "MINIO_ZONE_CREDENTIAL_TX_HASH: $MINIO_ZONE_CREDENTIAL_TX_HASH" >> $LOG_PATH/implementation.log
```