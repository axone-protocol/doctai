# DoctAI

> AI agent demo running on the [Axone](https://axone.xyz) protocol for collaborative medical diagnosis.

[![lint](https://img.shields.io/github/actions/workflow/status/axone-protocol/doctai/lint.yml?branch=main&label=lint&style=for-the-badge&logo=github)](https://github.com/axone-protocol/doctai/actions/workflows/lint.yml)
[![build](https://img.shields.io/github/actions/workflow/status/axone-protocol/doctai/build.yml?branch=main&label=build&style=for-the-badge&logo=github)](https://github.com/axone-protocol/doctai/actions/workflows/build.yml)
[![conventional commits](https://img.shields.io/badge/Conventional%20Commits-1.0.0-yellow.svg?style=for-the-badge&logo=conventionalcommits)](https://conventionalcommits.org)
[![contributor covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?style=for-the-badge)](https://github.com/axone-protocol/.github/blob/main/CODE_OF_CONDUCT.md)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg?style=for-the-badge)](https://opensource.org/licenses/BSD-3-Clause)
![favicon](public/favicon.ico)

## What is DoctAI?

> [!NOTE]
> 🥘 Still cooking. Not ready for prime time yet.

DoctAI is a webapp demo showcasing an AI agent running on top of the Axone protocol, enabling user interaction with a collaborative AI system for medical diagnosis.

Main features:

- Submit medical queries or data and receive AI-driven analysis.
- Demonstrate cross-lab collaboration via DoctAI.
- Highlight governance ensuring data ownership and control.
- Provide a functional, user-friendly demo interface.

## Getting Started

This is a [Next.js](https://nextjs.org) project bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

Be sure to have the following properly installed:

- [Node.js](https://nodejs.org/en/) `v22.15` ([jod](https://nodejs.org/en/blog/release/v22.15.0/))
- [pnpm](https://pnpm.io/) `v10.10`
- [Docker](https://www.docker.com/)

### Setup environment variables

Copy the example file and edit if needed:

```sh
cp .env.example .env
```

### Start the PostgreSQL database

Use Docker Compose to launch the [PostgreSQL](https://www.postgresql.org/) database:

```sh
docker compose up -d
```

If you want to use [Adminer](https://www.adminer.org/en/) (web UI for PostgreSQL), run with the debug profile:

```sh
docker compose --profile debug up -d
```

### Install dependencies

Install the required dependencies using `pnpm`:

```sh
pnpm install
```

### Start the development server

```sh
pnpm dev
```

Open <http://localhost:3000> in your browser to see the result.

## You want to get involved? 😍

Please check out Axone health files :

- [Contributing](https://github.com/axone-protocol/.github/blob/main/CONTRIBUTING.md)
- [Code of conduct](https://github.com/axone-protocol/.github/blob/main/CODE_OF_CONDUCT.md)
