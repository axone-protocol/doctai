# File: $WORK_DIR_AXONE/scripts/helpers.sh

# Add debug flag
DEBUG=false

# Function to log debug messages
debug_log() {
    if [ "$DEBUG" = true ]; then
        echo "DEBUG: $1"
    fi
}

# Function to check if a transaction exists and is processed
check_tx_exists() {
    local tx_hash=$1
    local node=$2
    local axoned_path=$3

    debug_log "Checking tx: $tx_hash"
    debug_log "Using node: $node"
    debug_log "Using axoned: $axoned_path"

    local result
    result=$($axoned_path query tx "$tx_hash" --node "$node" -o json 2>&1)
    debug_log "Query result: $result"

    if [[ $result == *"not found"* ]]; then
        debug_log "Transaction not found"
        return 1
    fi
    debug_log "Transaction found"
    return 0
}

# Function to wait for transaction to be processed
wait_for_tx() {
    local tx_hash=$1
    local node=$2
    local axoned_path=$3
    local max_attempts=${4:-30}
    local sleep_time=${5:-5}

    debug_log "Starting wait_for_tx for hash: $tx_hash"
    echo "Waiting for transaction $tx_hash to be processed..."

    $axoned_path query event-query-tx-for "$tx_hash" --node $node > /dev/null || true
    
    local attempt=1
    while [ $attempt -le $max_attempts ]; do
        debug_log "Attempt $attempt/$max_attempts"
        if check_tx_exists "$tx_hash" "$node" "$axoned_path"; then
            echo "Transaction found after $attempt attempts"
            return 0
        fi
        echo "Attempt $attempt/$max_attempts - Transaction not yet processed. Waiting ${sleep_time}s..."
        sleep $sleep_time
        attempt=$((attempt + 1))
    done

    echo "Error: Transaction not found after $max_attempts attempts"
    return 1
}

# Function to check transaction status and log details
check_tx_status() {
    local tx_hash=$1
    local node=$2
    local axoned_path=$3

    debug_log "Checking status for tx: $tx_hash"
    echo "Checking status for transaction: $tx_hash"
    
    local tx_result
    tx_result=$($axoned_path query tx "$tx_hash" --node "$node" -o json)
    debug_log "Raw tx result: $tx_result"
    
    local tx_code
    tx_code=$(echo "$tx_result" | jq -r '.code')
    debug_log "Transaction code: $tx_code"

    if [ "$tx_code" = "0" ]; then
        echo "✅ Transaction successful!"
        
        # Get triple count if exists
        local triple_count
        triple_count=$(echo "$tx_result" | jq -r '.events[] | select(.type=="wasm") | .attributes[] | select(.key=="triple_count") | .value')
        if [ ! -z "$triple_count" ]; then
            echo "📊 Triples inserted: $triple_count"
        fi

        # Get contract address if exists
        local contract_address
        contract_address=$(echo "$tx_result" | jq -r '.events[] | select(.type=="instantiate") | .attributes[] | select(.key=="_contract_address") | .value')
        if [ ! -z "$contract_address" ]; then
            echo "📝 Contract Address: $contract_address"
        fi

        # Get gas info
        local gas_used
        gas_used=$(echo "$tx_result" | jq -r '.gas_used')
        local gas_wanted
        gas_wanted=$(echo "$tx_result" | jq -r '.gas_wanted')
        echo "⛽ Gas used: $gas_used / $gas_wanted"

        # Print all wasm events for debugging
        echo "📋 WASM Events:"
        echo "$tx_result" | jq -r '.events[] | select(.type=="wasm") | .attributes[] | "\(.key): \(.value)"'
        
        return 0
    else
        echo "❌ Transaction failed with code: $tx_code"
        local error_log
        error_log=$(echo "$tx_result" | jq -r '.raw_log')
        echo "⚠️ Error: $error_log"
        return 1
    fi
}

# Combined function to wait and check status
wait_and_check_tx() {
    local tx_hash=$1
    local node=$2
    local axoned_path=$3
    local max_attempts=${4:-30}
    local sleep_time=${5:-5}

    debug_log "Starting wait_and_check_tx"
    debug_log "Hash: $tx_hash"
    debug_log "Node: $node"
    debug_log "Axoned: $axoned_path"

    if wait_for_tx "$tx_hash" "$node" "$axoned_path" "$max_attempts" "$sleep_time"; then
        check_tx_status "$tx_hash" "$node" "$axoned_path"
        return $?
    else
        return 1
    fi
}