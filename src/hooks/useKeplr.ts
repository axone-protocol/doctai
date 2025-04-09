'use client';
import { useState } from 'react';
import { connectKeplr } from '@/lib/keplr';

export function useKeplr() {
  const [address, setAddress] = useState<string | null>(null);

  const connect = async () => {
    const result = await connectKeplr();
    if (result) {
      setAddress(result.address);
    }
  };

  return { address, connect };
}
