import { getDidFromPubKeyBase64, verifyADR36Signature } from "@/lib/keplr/keplr";
import NextAuth from "next-auth";
import Credentials from "next-auth/providers/credentials";
import { queryIsContributor } from "@/lib/keplr/utils";
import { SESSION_MAX_AGE } from "@/lib/constants";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import { User } from "@/entities/User";

const handler = NextAuth({
    providers: [
        Credentials({
            name: "Keplr",
            credentials: {
                address: { label: "Wallet Address", type: "text" },
                signature: { label: "Signature", type: "text" },
                message: { label: "Message", type: "text" },
                pubKey: { label: "Public Key", type: "text" },
                // rememberMe: { label: "Remember Me", type: "boolean" },
            },
            authorize: async (credentials) => {
                const { address, signature, message, pubKey } =
                    credentials ?? {};
                //---------------------------------------
                console.log("[Auth] Authorize attempt with:", {
                    address,
                    signature: signature?.substring(0, 20) + "...",
                    message,
                    pubKey: pubKey?.substring(0, 20) + "...",
                });
                //---------------------------------------
                if (
                    typeof address !== "string" ||
                    typeof signature !== "string" ||
                    typeof message !== "string" ||
                    typeof pubKey !== "string" ||
                    // typeof rememberMe !== "boolean" ||
                    !address ||
                    !signature ||
                    !message ||
                    !pubKey
                ) {
                    console.warn("[Auth] Invalid credentials:", credentials);
                    return Promise.resolve(null);
                }
                //---------------------------------------
                try {
                    const isValid = verifyADR36Signature({
                        message,
                        address,
                        pubKeyBase64: pubKey,
                        signatureBase64: signature,
                    });
                    console.log(
                        "[Auth] Signature verification result:",
                        isValid
                    );
                    if (!isValid) return null;
                    //---------------------------------------
                    const userDID = getDidFromPubKeyBase64(pubKey);
                    //---------------------------------------
                    const isContributor = await queryIsContributor(userDID);
                    //---------------------------------------
                    const db = await PostgreSQLDatabaseService.getDataSource();
                    const repo = db.getRepository(User.name);
                    let user = await repo.findOneBy({ address });
                    if (user) {
                        user.userType = isContributor ? "contributor" : "guest";
                        user.userDID = userDID;
                        await repo.save(user); // update
                    } else {
                        user = repo.create({
                            address,
                            userType: isContributor ? "contributor" : "guest",
                            userDID,
                        });
                        await repo.save(user); // insert
                    }
                    //---------------------------------------
                    // const maxAge = rememberMe === "true" ? SESSION_MAX_AGE : 0;
                    //---------------------------------------
                    return {
                        id: address,
                        address,
                        userType: isContributor ? "contributor" : "guest",
                        userDID,
                        // maxAge,
                    };
                } catch (error) {
                    console.error("[Auth] Verification error:", error);
                    return null;
                }
            },
        }),
    ],
    callbacks: {
        async jwt({ token, user }) {
            if (
                user &&
                typeof user === "object" &&
                "address" in user &&
                "userType" in user &&
                "userDID" in user
                //  && "maxAge" in user
            ) {
                token.address = (user as any).address;
                token.userType = (user as any).userType;
                token.userDID = (user as any).userDID;
                // token.maxAge = (user as any).maxAge;
            }
            return token;
        },
        async session({ session, token }) {
            session.user = {
                ...(session.user || {}),
                address: token.address as string,
                userType: token.userType as "contributor" | "guest",
                userDID: token.userDID as string,
                // maxAge: token.maxAge as number, // Use maxAge from token
                // expires: new Date(Date.now() + (token.maxAge as number || 0) * 1000).toISOString(),
            };
            return session;
        },
    },
    session: { strategy: "jwt", maxAge: SESSION_MAX_AGE },
});

export { handler as GET, handler as POST };
