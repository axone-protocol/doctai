
# README MINIO STORAGE SERVICE

## Table of Contents

- [README MINIO STORAGE SERVICE](#readme-minio-storage-service)
  - [Table of Contents](#table-of-contents)
  - [Create Minio S3 Storage Service Description](#create-minio-s3-storage-service-description)
    - [Step 1: Set Env for Service Description Credential](#step-1-set-env-for-service-description-credential)
    - [Step 2: Key and DID for Service Credentials](#step-2-key-and-did-for-service-credentials)
    - [Step 3: Create Service Description Credential](#step-3-create-service-description-credential)
    - [Step 4: Sign and Submit Service Description Credential](#step-4-sign-and-submit-service-description-credential)
  - [Create Minio S3 Storage Service Governance](#create-minio-s3-storage-service-governance)
    - [Step 1: Set Env for Service Governance Credential](#step-1-set-env-for-service-governance-credential)
    - [Step 2: Create Service Governance Code](#step-2-create-service-governance-code)
    - [Step 3: Submit Service Governance Code](#step-3-submit-service-governance-code)
    - [Step 4: Create Service Governance Credentials](#step-4-create-service-governance-credentials)
    - [Step 5: Sign and Submit Service Governance Credentials](#step-5-sign-and-submit-service-governance-credentials)
    - [Step 7: Testing Service Governance Code](#step-7-testing-service-governance-code)
  - [Create Minio S3 Storage Service Zone Membership](#create-minio-s3-storage-service-zone-membership)
    - [Step 1: Create Service Zone Membership Credential](#step-1-create-service-zone-membership-credential)
    - [Step 4: Sign and Submit Service Zone Membership Credential](#step-4-sign-and-submit-service-zone-membership-credential)
  - [Querying Cognitarium](#querying-cognitarium)
    - [Get Zone DID from Minio Service DID using Zone Membership Credential](#get-zone-did-from-minio-service-did-using-zone-membership-credential)

## Create Minio S3 Storage Service Description

### Step 1: Set Env for Service Description Credential

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

```bash
export MINIO_DESC_PATH=$WORK_DIR_AXONE/files/minio/description
mkdir -p $MINIO_DESC_PATH

export MINIO_WALLET=minio-wallet

echo "MINIO_DESC_PATH=\"$MINIO_DESC_PATH\""
echo "MINIO_DESC_PATH=\"$MINIO_DESC_PATH\"" >> $LOG_PATH/implementation.log
echo "MINIO_WALLET=\"$MINIO_WALLET\""
echo "MINIO_WALLET=\"$MINIO_WALLET\"" >> $LOG_PATH/implementation.log

```

### Step 2: Key and DID for Service Credentials

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

echo "MINIO_ADDRESS=\"$MINIO_ADDRESS\""
echo "MINIO_ADDRESS=\"$MINIO_ADDRESS\"" >> $LOG_PATH/implementation.log
echo "MINIO_DID=\"$MINIO_DID\""
echo "MINIO_DID=\"$MINIO_DID\"" >> $LOG_PATH/implementation.log
echo "MINIO_PHRASE=\"$MINIO_PHRASE\""
echo "MINIO_PHRASE=\"$MINIO_PHRASE\"" >> $LOG_PATH/implementation.log
```

### Step 3: Create Service Description Credential

```bash
export MINIO_CRED_DESCRIPTION_ID=$(uuidgen)
echo "MINIO_CRED_DESCRIPTION_ID=\"$MINIO_CRED_DESCRIPTION_ID\""
echo "MINIO_CRED_DESCRIPTION_ID=\"$MINIO_CRED_DESCRIPTION_ID\"" >> $LOG_PATH/implementation.log

# Create service description
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

### Step 4: Sign and Submit Service Description Credential

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

## Create Minio S3 Storage Service Governance

### Step 1: Set Env for Service Governance Credential

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
# Identidad a testear
export TEST_DID="did:key:z16DtrNpHS569CKDUFaXGzAMnLdMoaanxHqqsPnd91ZnyufLL"
export TEST_ACTION="store"     # acción a verificar

# Contrato law-stone
export LAW_STONE_ADDR=$MINIO_GOV_CODE_ADDR

# ------------------------------
# ASK via gRPC (tell_permitted_actions)
# ------------------------------

export PROLOG_QUERY="tell_permitted_actions('$TEST_DID', Actions)."
export ENCODED_QUERY=$(echo -n "{\"ask\":{\"query\":\"$PROLOG_QUERY\"}}" | base64 -w 0)

echo "🔍 GRPC tell_permitted_actions for DID=\"$TEST_DID\""

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

## Create Minio S3 Storage Service Zone Membership

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

### Get Zone DID from Minio Service DID using Zone Membership Credential

This query retrieves the Zone DID associated with a given service (Minio) DID based on the Zone Membership Credential.

```bash

export QUERY=$(jq -n --arg minioDid "$MINIO_DID" '{
  select: {
    query: {
      prefixes: [],
      select: [
        { variable: "credential" },
        { variable: "predicate" },
        { variable: "object" }
      ],
      where: {
        bgp: {
          patterns: [
            {
              subject: { variable: "credential" },
              predicate: { variable: "predicate" },
              object: { variable: "object" }
            },
            {
              subject: { variable: "credential" },
              predicate: { named_node: { full: "dataverse:credential:body#subject" } },
              object: { node: { named_node: { full: $minioDid } } }
            }
          ]
        }
      }
    }
  }
}')

# Ejecutar la consulta
$AXONED_PATH query wasm contract-state smart $COGNITARIUM_ADDR "$QUERY" \
  --node $AXONE_NODE_RPC --output json | jq '.data.results.bindings'









export QUERY=$(jq -n --arg minioDid "$MINIO_DID" '{
  select: {
    query: {
      prefixes: [],
      select: [{ variable: "zoneDid" }],
      where: {
        bgp: {
          patterns: [
            {
              subject: { variable: "credential" },
              predicate: { named_node: { full: "dataverse:credential:body#subject" } },
              object: { node: { named_node: { full: $minioDid } } }
            },
            {
              subject: { variable: "credential" },
              predicate: { named_node: { full: "dataverse:credential:body#type" } },
              object: { node: { named_node: { full: "https://w3id.org/axone/ontology/$ONTOLOGY_NEXT_VERSION/schema/credential/zone/membership/ZoneMembershipCredential" } } }
            },
            {
              subject: { variable: "credential" },
              predicate: { named_node: { full: "dataverse:credential:body#claim" } },
              object: { variable: "claim" }
            },
            {
              subject: { variable: "claim" },
              predicate: { named_node: { full: "https://w3id.org/axone/ontology/$ONTOLOGY_NEXT_VERSION/schema/credential/zone/membership/inZone" } },
              object: { variable: "zoneDid" }
            }
          ]
        }
      }
    }
  }
}')

# Execute the query
$AXONED_PATH query wasm contract-state smart $COGNITARIUM_ADDR "$QUERY" --node $AXONE_NODE_RPC --output json | jq -r '.data.results.bindings[0].zoneDid.value.full'


# GET ZONE DID FROM MINIO DID

export QUERY=$(jq -n --arg minioDid "$MINIO_DID" '{
  select: {
    query: {
      prefixes: [],
      select: [{ variable: "zoneDid" }],
      where: {
        lateral_join: {
          left: {
            bgp: {
              patterns: [
                {
                  subject: { variable: "credential" },
                  predicate: { named_node: { full: "dataverse:credential:body#subject" } },
                  object: { node: { named_node: { full: $minioDid } } }
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
                  object: { variable: "zoneDid" }
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