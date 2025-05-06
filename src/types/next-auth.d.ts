import NextAuth, { DefaultSession, DefaultUser } from "next-auth";
import { SignInResponse } from "next-auth/react";
import { NextApiRequest, NextApiResponse } from "next";

declare module "next-auth" {
    interface Session {
        user: {
            id: string;
            address: string;
            userType: "contributor" | "guest";
            userDID: string;
            // maxAge: number;
            // expires: string;
        };
    }

    interface JWT {
        id: string;
        address: string;
        userType: "contributor" | "guest";
        userDID: string;
        // maxAge: number;
        // expires: string;
    }
}
