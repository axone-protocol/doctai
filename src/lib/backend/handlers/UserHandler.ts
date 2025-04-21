// lib/backend/handlers/UserHandler.ts
import { User } from "@/entities/User";
import { BaseEntityServer, registerEntity } from "../BaseEntityServer";

export const UserHandler: BaseEntityServer<User> = {
    entityName: User.name,

    toDTO: (user) => ({
        id: user.id,
        address: user.address,
        userType: user.userType,
        createdAt: user.createdAt.toISOString(),
        lastLogin: user.lastLogin.toISOString(),
    }),

    fromDTO: (dto) => {
        const user = new User();
        user.id = dto.id;
        user.address = dto.address;
        user.userType = dto.userType;
        // user.createdAt = new Date(dto.createdAt);
        // user.lastLogin = new Date(dto.lastLogin);
        return user;
    },
};

registerEntity(UserHandler);
