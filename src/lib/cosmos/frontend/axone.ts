import {
    AXONE_NODE_RPC,
    COGNITARIUM_ADDR
} from "@/lib/constants";
import { CosmWasmClient } from "@cosmjs/cosmwasm-stargate";

export async function fetchGovAddressFromDID(credDID: string): Promise<string> {
    try {
        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);

        const query = {
            select: {
                query: {
                    prefixes: [],
                    select: [
                        { variable: "credential" },
                        { variable: "p" },
                        { variable: "o" },
                        { variable: "issuanceDate" },
                    ],
                    where: {
                        lateral_join: {
                            left: {
                                bgp: {
                                    patterns: [
                                        {
                                            subject: { variable: "credential" },
                                            predicate: {
                                                named_node: {
                                                    full: "dataverse:credential:body#subject",
                                                },
                                            },
                                            object: {
                                                node: {
                                                    named_node: {
                                                        full: credDID,
                                                    },
                                                },
                                            },
                                        },
                                        {
                                            subject: { variable: "credential" },
                                            predicate: {
                                                named_node: {
                                                    full: "dataverse:credential:body#type",
                                                },
                                            },
                                            object: {
                                                node: {
                                                    named_node: {
                                                        full: "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/GovernanceTextCredential",
                                                    },
                                                },
                                            },
                                        },
                                        {
                                            subject: { variable: "credential" },
                                            predicate: {
                                                variable:
                                                    "dataverse:credential:body#claim",
                                            },
                                            object: { variable: "claim" },
                                        },
                                        {
                                            subject: { variable: "claim" },
                                            predicate: {
                                                named_node: {
                                                    full: "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/isGovernedBy",
                                                },
                                            },
                                            object: { variable: "governance" },
                                        },
                                        {
                                            subject: { variable: "credential" },
                                            predicate: {
                                                named_node: {
                                                    full: "dataverse:credential:body#validFrom",
                                                },
                                            },
                                            object: {
                                                variable: "issuanceDate",
                                            },
                                        },
                                    ],
                                },
                            },
                            right: {
                                bgp: {
                                    patterns: [
                                        {
                                            subject: { variable: "governance" },
                                            predicate: { variable: "p" },
                                            object: { variable: "o" },
                                        },
                                    ],
                                },
                            },
                        },
                    },
                },
            },
        };

        const response = await client.queryContractSmart(
            COGNITARIUM_ADDR,
            query
        );

        const bindings = response?.results?.bindings;

        if (!bindings || bindings.length === 0) {
            throw new Error(
                "No governance bindings found for the provided DID"
            );
        }

        // Filtramos solo bindings de fromGovernance
        const governanceBindings = bindings.filter(
            (binding: any) =>
                binding.p?.value?.full ===
                "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/fromGovernance"
        );

        if (governanceBindings.length === 0) {
            throw new Error("No fromGovernance fields found in the bindings");
        }

        // Ordenamos por issuanceDate si existe, si no usamos credential ID como fallback
        governanceBindings.sort((a: any, b: any) => {
            const aDate = new Date(a.issuanceDate?.value || "").getTime();
            const bDate = new Date(b.issuanceDate?.value || "").getTime();

            if (!isNaN(aDate) && !isNaN(bDate)) {
                return bDate - aDate; // más reciente primero
            }

            // fallback por credential ID
            const credA = a.credential?.value?.full || "";
            const credB = b.credential?.value?.full || "";
            return credB.localeCompare(credA);
        });

        // console.log(
        //     "fetchGovAddressFromDID → governanceBindings:",
        //     JSON.stringify(governanceBindings, null, 2)
        // );

        const lastGovernance = governanceBindings[0];
        const fromGovernanceURI = lastGovernance.o?.value?.full;

        if (typeof fromGovernanceURI !== "string") {
            throw new Error("Invalid fromGovernance value format");
        }

        const matches = fromGovernanceURI.match(/cosmwasm:law-stone:([^?]+)/);
        if (!matches) {
            throw new Error("Unable to parse law-stone address");
        }

        return matches[1]; // El governance address final limpio
    } catch (error) {
        console.error("fetchZoneGovAddress error:", error);
        return Promise.reject(error);
    }
}

export async function fetchGovCodeFromAddress(
    contractAddress: string
): Promise<string> {
    try {
        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);

        const query = {
            program_code: {},
        };

        const response = await client.queryContractSmart(
            contractAddress,
            query
        );

        const base64Encoded = response;
        if (!base64Encoded || typeof base64Encoded !== "string") {
            throw new Error("Invalid base64 program_code response");
        }

        // Decodificamos el base64 a texto
        const decoded = Buffer.from(base64Encoded, "base64").toString("utf-8");
        return decoded;
    } catch (error) {
        console.error("fetchPrologFromAddress error:", error);
        return Promise.reject("Failed to fetch or decode Prolog program");
    }
}

export async function fetchZoneDIDFromMinIODID(
    minioDID: string
): Promise<string> {
    const client = await CosmWasmClient.connect(AXONE_NODE_RPC);

    const query = {
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
                                        predicate: {
                                            named_node: {
                                                full: "dataverse:credential:body#subject",
                                            },
                                        },
                                        object: {
                                            node: {
                                                named_node: {
                                                    full: minioDID,
                                                },
                                            },
                                        },
                                    },
                                    {
                                        subject: { variable: "credential" },
                                        predicate: {
                                            named_node: {
                                                full: "dataverse:credential:body#type",
                                            },
                                        },
                                        object: {
                                            node: {
                                                named_node: {
                                                    full: "https://w3id.org/axone/ontology/vnext/schema/credential/zone/membership/ZoneMembershipCredential",
                                                },
                                            },
                                        },
                                    },
                                    {
                                        subject: { variable: "credential" },
                                        predicate: {
                                            named_node: {
                                                full: "dataverse:credential:body#claim",
                                            },
                                        },
                                        object: { variable: "claim" },
                                    },
                                ],
                            },
                        },
                        right: {
                            bgp: {
                                patterns: [
                                    {
                                        subject: { variable: "claim" },
                                        predicate: {
                                            named_node: {
                                                full: "https://w3id.org/axone/ontology/vnext/schema/credential/zone/membership/inZone",
                                            },
                                        },
                                        object: { variable: "zoneDID" },
                                    },
                                ],
                            },
                        },
                    },
                },
            },
        },
    };

    const result = await client.queryContractSmart(COGNITARIUM_ADDR, query);
    return result?.results?.bindings?.[0]?.zoneDID?.value?.full;
}
