// digitalbazaar.d.ts

declare module '@digitalbazaar/ed25519-verification-key-2020' {
  export class Ed25519VerificationKey2020 {
    static from(options: any): Promise<any>;
  }
}

declare module '@digitalbazaar/ed25519-signature-2020' {
  export class Ed25519Signature2020 {
    constructor(options: any);
  }
}

// src/types/digitalbazaar-vc.d.ts
declare module '@digitalbazaar/vc' {
  import type { LinkedDataSignature } from 'jsonld-signatures';
  import type { DocumentLoader } from 'jsonld-signatures';
  import type { VerifiableCredential } from 'vc-js';

  export function issue(options: {
    credential: any;
    suite: LinkedDataSignature;
    purpose?: any;
    documentLoader?: DocumentLoader;
    now?: string | Date;
    maxClockSkew?: number;
  }): Promise<VerifiableCredential>;

  export function derive(options: {
    verifiableCredential: any;
    suite: LinkedDataSignature;
    documentLoader?: DocumentLoader;
  }): Promise<any>;

  export function verify(options: any): Promise<any>;
  export function verifyCredential(options: any): Promise<any>;
  export function createPresentation(options: any): any;
  export function signPresentation(options: any): Promise<any>;
}


declare module '@digitalbazaar/data-integrity-context' {
  export const documentLoader: (url: string) => any;
}


declare module '@digitalbazaar/ed25519-multikey' {
  export function fromJwk(opts: any): Promise<any>;
  export function from(key: any): Promise<any>;
  export function toJwk(opts: any): Promise<any>;
  export function generate(opts?: any): Promise<any>;
}

declare module '@digitalbazaar/did-method-key' {
  export function driver(): any;
}

declare module '@digitalbazaar/data-integrity' {
  export class DataIntegrityProof {
    constructor(opts: any);
  }
}
