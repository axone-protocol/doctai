// hooks/useAuth.ts
import { useEffect, useState } from "react";
import { useSession } from "next-auth/react";
import { useRouter, usePathname } from "next/navigation";
import { useUserStore } from "@/store/useUserStore";
import { fetchEntities } from "@/lib/frontend/api";
import { User } from "@/entities/User";

export function useAuth({ autoRedirect = true } = {}) {
    const { data: session, status } = useSession();
    const {
        address,
        userType,
        setAddress,
        setUserType,
        setUser,
        setUserDID,
        reset,
    } = useUserStore();
    const router = useRouter();
    const pathname = usePathname();
    const [initialized, setInitialized] = useState(false);

    useEffect(() => {
        if (status === "loading") return;
        const hydrateUser = async () => {
            if (session?.user?.address) {
                // Use generic entity filter to fetch user from DB
                const users = await fetchEntities<User>(User.name, {
                    filter: { address: session.user.address },
                });
                const dbUser = users[0];

                if (dbUser) {
                    setAddress(session.user.address);
                    setUserType(session.user.userType);
                    setUserDID(session.user.userDID);
                    setUser(dbUser);
                }
            } else if (autoRedirect && pathname !== "/login") {
                router.replace("/login");
            }

            setInitialized(true);
        };

        hydrateUser();
        setInitialized(true);
    }, [status, session, pathname]);

    return {
        address,
        userType,
        isAuthenticated: !!address,
        initialized,
        setAddress,
        setUserType,
        setUserDID,
        reset,
    };
}
