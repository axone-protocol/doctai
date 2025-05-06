import axios from "axios";
import * as jsonld from "jsonld";

// Definimos interfaces propias sin depender de jsonld-spec
export interface RemoteDocument {
    contextUrl: string | undefined;
    document: any;
    documentUrl: string;
}

export interface SchemaMap {
    [url: string]: any;
}
/**
 * Creates a document loader compatible with the jsonld types
 * @param schemaMap - Map of known contexts and schemas
 * @returns A function documentLoader with the correct signature for jsonld
 */
export function getDocumentLoader(
    schemaMap: SchemaMap = {}
): (
    url: string,
    callback?: (err: Error, remoteDoc: RemoteDocument) => void
) => Promise<RemoteDocument> {
    // Return a function compatible with jsonld.normalize
    return async function (
        url: string,
        callback?: (err: Error, remoteDoc: RemoteDocument) => void
    ): Promise<RemoteDocument> {
        let result: RemoteDocument;

        try {
            console.log(`[getDocumentLoader] Requested URL: ${url}`);

            // Check if URL is in the known schema map
            if (schemaMap[url]) {
                console.log(
                    `[getDocumentLoader] Found context in schemaMap for: ${url}`
                );

                result = {
                    contextUrl: undefined,
                    document: schemaMap[url],
                    documentUrl: url,
                };
            } else {
                console.log(
                    `[getDocumentLoader] Fetching document from network: ${url}`
                );

                const response = await axios.get(url, {
                    headers: {
                        Accept: "application/ld+json, application/json",
                    },
                });

                console.log(
                    `[getDocumentLoader] Successfully fetched external context for: ${url}`
                );

                result = {
                    contextUrl: undefined,
                    document: response.data,
                    documentUrl: url,
                };
            }

            // Call the callback if provided
            if (callback) {
                console.log(
                    `[getDocumentLoader] Calling callback for successful load: ${url}`
                );
                callback(new Error(""), result); // Use non-null error to satisfy signature
            }

            return result;
        } catch (error) {
            console.error(
                `[getDocumentLoader] Failed to load context from: ${url}`
            );
            console.error(`[getDocumentLoader] Error details:`, error);

            // Call the callback if provided, with error
            if (callback) {
                callback(
                    error instanceof Error ? error : new Error(String(error)),
                    {
                        contextUrl: undefined,
                        document: {},
                        documentUrl: url,
                    }
                );
            }

            throw error instanceof Error ? error : new Error(String(error));
        }
    };
}

/**
 * Función para normalizar un documento JSON-LD con tipos correctos
 * @param document - Documento a normalizar
 * @param schemaMap - Mapa de contextos conocidos
 * @returns Datos normalizados en formato n-quads
 */
export async function normalizeJsonLd(
    document: any,
    schemaMap: SchemaMap = {}
): Promise<string> {
    const options = {
        algorithm: "URDNA2015" as const,
        format: "application/n-quads" as const,
        documentLoader: getDocumentLoader(schemaMap),
    };

    return jsonld.normalize(document, options);
}
/**
 * Converts a credential to N-Quads format
 * @param credential Credential to convert
 * @returns N-Quads representation of the credential
 */

export async function convertToNQuads(credential: any): Promise<string> {
    try {
        console.log(
            `[convertToNQuads] Converting credential to N-Quads format`
        );

        const result = await jsonld.toRDF(credential, {
            format: "application/n-quads",
        });

        const resultStr =
            typeof result === "string" ? result : result.toString();
        console.log(
            `[convertToNQuads] Conversion successful, N-Quads length: ${resultStr.length}`
        );

        return resultStr;
    } catch (error) {
        console.error("[convertToNQuads] Error converting to N-Quads:", error);
        throw new Error("Failed to convert credential to N-Quads format");
    }
}
