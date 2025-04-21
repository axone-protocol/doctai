import NextAuth, { DefaultSession, DefaultUser } from 'next-auth';
import { SignInResponse } from 'next-auth/react';
import { NextApiRequest, NextApiResponse } from 'next';

declare module "next-auth" {
    interface Session {
        user: {
            address: string;
            userType: "contributor" | "guest";
            // maxAge: number;
            // expires: string;
        };
    }

    interface JWT {
        address: string;
        userType: "contributor" | "guest";
        // maxAge: number;
        // expires: string;
    }
}
