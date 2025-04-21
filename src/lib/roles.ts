import { CosmWasmClient } from "@cosmjs/cosmwasm-stargate";
import { CONTRACT_ADDRESS_HEARTH_LABS } from "./constants"; // define esto según tu deploy

const RPC_URL = "https://rpc.testnet.okp4.network"; // o tu RPC custom

export async function queryIsContributor(address: string): Promise<boolean> {
    //   const client = await CosmWasmClient.connect(RPC_URL);
    //   const result = await client.queryContractSmart(CONTRACT_ADDRESS_HEARTH_LABS, {
    //     is_contributor: { address }
    //   });
    //   return result.is === true;
    return true;
}
