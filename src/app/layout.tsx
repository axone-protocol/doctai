// src/app/layout.tsx
import './globals.scss';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'DoctAI',
  description: 'Decentralized AI Health Assistant',
  icons: {
    icon: '/favicon.ico',
    apple: '/apple-touch-icon.png',
  },
  manifest: '/manifest.json',
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
