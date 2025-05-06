// src/lib/frontend/axone/fetch.ts

import {
  fetchGovCodeFromAddress,
  fetchGovAddressFromDID,
  fetchZoneDIDFromMinIODID,
} from "@/lib/cosmos/frontend/axone";

/**
 * Loads full governance info starting from the MINIO service DID.
 */
export async function preloadAxoneInfoFromMinioDid(minioDID: string) {
  // Step 1: Get the zone DID that governs the MINIO service
  const zoneDID = await fetchZoneDIDFromMinIODID(minioDID);
  if (!zoneDID) throw new Error("Zone DID not found from MINIO_DID");

  // Step 2: Get the governance contract address of the zone
  const zoneGovAddress = await fetchGovAddressFromDID(zoneDID);
  if (!zoneGovAddress) throw new Error("Zone governance address not found");

  // Step 3: Fetch the Prolog code for the zone governance contract
  const zoneGovCode = await fetchGovCodeFromAddress(zoneGovAddress);

  // Step 4: Get the governance contract address directly for MINIO
  const minioGovAddress = await fetchGovAddressFromDID(minioDID);
  if (!minioGovAddress) throw new Error("MINIO governance address not found");

  // Step 5: Fetch the Prolog code for the MINIO governance contract
  const minioGovCode = await fetchGovCodeFromAddress(minioGovAddress);

  return {
    zoneDID,
    minioDID,
    zoneGovAddress,
    minioGovAddress,
    zoneGovCode,
    minioGovCode,
  };
}
