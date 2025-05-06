import type { NextConfig } from "next";
const webpack = require("webpack");

const nextConfig: NextConfig = {
    //-------------
    // PRODUCTION
    // swcMinify: true,
    // reactStrictMode: true,
    //-------------
    // DEV
    reactStrictMode: false,
    //-------------
    /* config options here */
    webpack: (config, { dev, isServer }) => {
        config.ignoreWarnings = [
            {
                module: /typeorm/,
                message: /Module not found|dependency is an expression/,
            },
        ];

        // 🔕 Silencia critical dependency warning
        config.module.exprContextCritical = false;

        config.resolve.fallback = {
            ...config.resolve.fallback,
            "rdf-canonize-native": false,
        };

        return config;
    },
};

export default nextConfig;
