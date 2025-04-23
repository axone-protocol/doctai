import { User } from "@/entities/User";
import { create } from "zustand";

export type UserType = "contributor" | "guest";

interface UserState {
    address: string | undefined;
    userType: UserType | undefined;
    userDID: string | undefined;
    user: User | undefined;
    setAddress: (address: string | undefined) => void;
    setUserType: (type: UserType | undefined) => void;
    setUserDID: (userDID: string | undefined) => void;
    setUser: (user: User | undefined) => void;
    reset: () => void;
}

export const useUserStore = create<UserState>((set) => ({
    address: undefined,
    userType: undefined,
    userDID: undefined,
    user: undefined,
    setAddress: (address) => set({ address }),
    setUserType: (userType) => set({ userType }),
    setUserDID: (userDID) => set({ userDID }),
    setUser: (user) => set({ user }),
    reset: () =>
        set({
            address: undefined,
            userType: undefined,
            userDID: undefined,
            user: undefined,
        }),
}));
