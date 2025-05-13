// lib/backend/relations.ts

// Hardcoded mapping from EntityName to property name used in TypeORM relations
const relationMap: Record<string, Record<string, string>> = {
    Chat: {
        ChatMessage: "chatMessages",
        User: "user",
    },
    ChatMessage: {
        Chat: "chat",
    },
    User: {
        Chat: "chats",
    },
};

// Resolve a relation property name given an entity and target
export function getRelationPropertyName(
    entity: string,
    relation: string
): string {
    return relationMap[entity]?.[relation] ?? relation;
}

export function getRelationKey(
    entity: string,
    related: string
): string {
    return getRelationPropertyName(entity, related);
}

export function getEntityNameFromRelationProperty(
    parentEntity: string,
    propertyName: string
): string | undefined {
    const reverseMap = Object.entries(relationMap[parentEntity] || {}).find(
        ([targetEntity, prop]) => prop === propertyName
    );
    return reverseMap?.[0];
}
