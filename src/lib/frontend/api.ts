// lib/frontend/api.ts

// Utility to convert a query object to URLSearchParams string
function buildQuery(params: Record<string, any>) {
    const search = new URLSearchParams();
    for (const key in params) {
        const value = params[key];
        if (typeof value === "object") {
            for (const subKey in value) {
                search.set(`filter[${subKey}]`, value[subKey]);
            }
        } else {
            search.set(key, value);
        }
    }
    return search.toString();
}

export async function fetchEntities<T>(
    entity: string,
    query: Record<string, any> = {}
): Promise<T[]> {
    const url = `/api/entities/${entity}?${buildQuery(query)}`;
    const res = await fetch(url);
    return await res.json();
}

export async function fetchEntityById<T>(
    entity: string,
    id: string
): Promise<T> {
    const res = await fetch(`/api/entities/${entity}/${id}`);
    return await res.json();
}

export async function fetchEntityRelation<T>(
    entity: string,
    id: string,
    relation: string
): Promise<T[]> {
    const res = await fetch(`/api/entities/${entity}/${id}/${relation}`);
    return await res.json();
}

export async function createEntity<T>(
    entity: string,
    data: Partial<T>
): Promise<T> {
    const res = await fetch(`/api/entities/${entity}`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(data),
    });
    return await res.json();
}

export async function updateEntity<T>(
    entity: string,
    id: string,
    data: Partial<T>
): Promise<T> {
    const res = await fetch(`/api/entities/${entity}/${id}`, {
        method: "PUT",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(data),
    });
    return await res.json();
}

export async function deleteEntity(entity: string, id: string): Promise<void> {
    await fetch(`/api/entities/${entity}/${id}`, {
        method: "DELETE",
    });
}
