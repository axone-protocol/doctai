import { User } from "@/entities/User";
import { create } from "zustand";

export type UserType = "contributor" | "guest" ;

interface UserState {
    address: string | undefined;
    userType: UserType | undefined;
    user: User | undefined;
    setAddress: (address: string | undefined) => void;
    setUserType: (type: UserType | undefined) => void;
    setUser: (user: User | undefined) => void;
    reset: () => void;
}

export const useUserStore = create<UserState>((set) => ({
    address: undefined,
    userType: undefined,
    user: undefined,
    setAddress: (address) => set({ address }),
    setUserType: (userType) => set({ userType }),
    setUser: (user) => set({ user }),
    reset: () => set({ address: undefined, userType: undefined , user: undefined}),
}));
