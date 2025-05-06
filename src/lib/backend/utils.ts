import { getToken } from "next-auth/jwt";
import { NextRequest } from "next/server";

export function formatTableName(baseName: string): string {
    return baseName;
    // return baseName
    //     .toLowerCase()
    //     .split(" ")
    //     .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    //     .join("");
}

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
