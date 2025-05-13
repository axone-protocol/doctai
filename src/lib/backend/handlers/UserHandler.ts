// lib/backend/handlers/UserHandler.ts
import { User } from "@/entities/User";
import { BaseEntityServer, registerEntity } from "../BaseEntityServer";
import { ENTITY_NAMES } from "@/lib/constants";

export const UserHandler: BaseEntityServer<User> = {
    entityName: ENTITY_NAMES.User,

    toDTO: (user) => ({
        id: user.id,
        address: user.address,
        userType: user.userType,
        userDID: user.userDID,
        createdAt: user.createdAt.toISOString(),
        lastLogin: user.lastLogin.toISOString(),
    }),

    fromDTO: (dto) => {
        const user = new User();
        user.id = dto.id;
        user.address = dto.address;
        user.userType = dto.userType;
        user.userDID = dto.userDID;
        // user.createdAt = new Date(dto.createdAt);
        // user.lastLogin = new Date(dto.lastLogin);
        return user;
    },
};

registerEntity(UserHandler);
