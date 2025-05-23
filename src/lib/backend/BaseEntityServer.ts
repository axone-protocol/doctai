// lib/backend/BaseEntityServer.ts
export interface BaseEntityServer<T> {
    entityName: string;
    toDTO(entity: T): any;
    fromDTO(input: any): T;
    relationPropertyNameFor?: (relatedEntityName: string) => string;
}

const entityRegistry: Map<string, BaseEntityServer<any>> = new Map();

export const registerEntity = <T>(handler: BaseEntityServer<T>) => {
    entityRegistry.set(handler.entityName, handler);
};

export const getEntityHandler = (name: string): BaseEntityServer<any> => {
    const handler = entityRegistry.get(name);
    if (!handler) throw new Error(`Entity handler not found for "${name}"`);
    return handler;
};

// Add this function to get the entity class by name
export const getEntityByName = (name: string): any => {
    try {
        // First try to get from direct import
        const entityModule = require(`@/entities/${name}`);
        return entityModule[name];
    } catch (e) {
        // Fallback to using the entity name as is
        return name;
    }
};
