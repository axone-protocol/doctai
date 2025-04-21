"use client";
import { useAuth } from "@/hooks/useAuth";

export default function AuthGuard({ children }: { children: React.ReactNode }) {
    const { initialized, isAuthenticated } = useAuth();

    if (!initialized) return null;
    if (!isAuthenticated) return null;

    return <>{children}</>;
}
