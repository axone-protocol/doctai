#!/bin/bash

set -e

SCHEMA_DIR="./src/lib/cosmos/backend/schemas"
SCHEMA_MAP="$SCHEMA_DIR/schemaMap.ts"

# List of context URLs and their target local filenames
declare -A CONTEXTS
CONTEXTS["https://www.w3.org/2018/credentials/v1"]="credentials-v1.json"
CONTEXTS["https://w3id.org/axone/ontology/v4/schema/credential/dataset/description/"]="axone-dataset-desc.json"
CONTEXTS["https://w3id.org/security/suites/secp256k1-2019/v1"]="secp256k1-2019.json"
CONTEXTS["https://w3id.org/security/v1"]="security-v1.json"

# Ensure directory exists
mkdir -p "$SCHEMA_DIR"

echo "📥 Downloading contexts..."
for URL in "${!CONTEXTS[@]}"; do
    FILE="${CONTEXTS[$URL]}"
    DEST="$SCHEMA_DIR/$FILE"
    echo "🔹 $URL → $DEST"
    
    curl -sSL -H "Accept: application/ld+json, application/json" "$URL" -o "$DEST"
    
    # Check download success
    if [[ ! -s "$DEST" ]]; then
        echo "❌ Failed to download or empty file for $URL"
        exit 1
    fi
done

echo "🧠 Generating schemaMap.ts..."
{
    echo "/* AUTO-GENERATED: DO NOT EDIT MANUALLY */"
    echo ""
    for FILE in "${CONTEXTS[@]}"; do
        VAR_NAME=$(basename "$FILE" .json | tr '-' '_')
        echo "import $VAR_NAME from './$(basename "$FILE")';"
    done
    echo ""
    echo "export const schemaMap = {"
    for URL in "${!CONTEXTS[@]}"; do
        VAR_NAME=$(basename "${CONTEXTS[$URL]}" .json | tr '-' '_')
        echo "  \"$URL\": $VAR_NAME,"
    done
    echo "};"
} > "$SCHEMA_MAP"

echo "✅ Done. All schemas are stored in: $SCHEMA_DIR"
echo "🔗 schemaMap.ts created: $SCHEMA_MAP"
