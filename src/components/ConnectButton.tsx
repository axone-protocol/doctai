// components/ConnectButton.tsx
'use client';
import { useState } from 'react';
import { connectKeplr } from '@/lib/keplr';

export default function ConnectButton() {
  const [address, setAddress] = useState<string | null>(null);

  const handleConnect = async () => {
    const result = await connectKeplr();
    if (result) {
      setAddress(result.address);
    }
  };

  return (
    <button onClick={handleConnect}>
      {address ? `Connected: ${address}` : 'Connect Wallet'}
    </button>
  );
}
