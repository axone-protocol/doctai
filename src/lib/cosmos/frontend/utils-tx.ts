import axios from "axios";
import {
  AXONE_DENOM,
  AXONE_NODE_RPC,
  AXON_GAS_PRICE,
  CHAIN_ID,
  DEFAULT_GAS_LIMIT,
} from "@/lib/constants";
import { StdFee } from "@cosmjs/amino";
import { toUtf8 } from "@cosmjs/encoding";
import { SigningStargateClient, DeliverTxResponse } from "@cosmjs/stargate";
import { SigningCosmWasmClient } from "@cosmjs/cosmwasm-stargate";
import { getKeplr } from "./keplr";

/**
 * Performs guest payment by requesting datasetDID and building MsgSend
 */
export async function performGuestPayment(userAddress: string): Promise<string> {
  console.log("[performGuestPayment] Preparing guest payment...");

  const keplr = getKeplr();
  const signer = await keplr.getOfflineSigner(CHAIN_ID);
  const [{ address }] = await signer.getAccounts();

  const { data } = await axios.post("/api/entities/upload/prepare-payment-tx");
  const { datasetDID, payment } = data;

  const fee: StdFee = {
    amount: [{ denom: AXONE_DENOM, amount: "5000" }],
    gas: DEFAULT_GAS_LIMIT.toString(),
  };

  const bankMsg = {
    typeUrl: "/cosmos.bank.v1beta1.MsgSend",
    value: {
      fromAddress: payment.fromAddress,
      toAddress: payment.toAddress,
      amount: payment.amount,
    },
  };

  const client = await SigningStargateClient.connectWithSigner(AXONE_NODE_RPC, signer);
  const result = await client.signAndBroadcast(address, [bankMsg], fee, payment.memo);

  if (result.code !== 0) {
    console.error("[performGuestPayment] Payment failed:", result.rawLog);
    throw new Error("Guest payment transaction failed");
  }

  console.log("[performGuestPayment] ✅ Payment successful:", result.transactionHash);
  return datasetDID;
}

/**
 * Confirms upload by calling the backend
 */
export async function confirmCredentialTx(txHash: string, file: File): Promise<any> {
  console.log("[confirmCredentialTx] Confirming credential transaction with backend...");

  const form = new FormData();
  form.append("txHash", txHash);
  form.append("file", file);

  const { data } = await axios.post("/api/entities/upload/confirm-tx", form);
  return data;
}
