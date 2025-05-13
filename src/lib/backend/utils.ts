import { getToken } from "next-auth/jwt";
import { NextRequest } from "next/server";

export async function getUserFromRequest(req: NextRequest) {
    const token = await getToken({ req });
    if (
        !token ||
        !token.id ||
        !token.userDID ||
        !token.address ||
        !token.userType
    ) {
        throw new Error("Unauthorized or malformed token");
    }

    return {
        id: token.id as string,
        userDID: token.userDID as string,
        address: token.address as string,
        userType: token.userType as "contributor" | "guest",
    };
}
