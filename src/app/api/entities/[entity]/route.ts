// app/api/entities/[entity]/route.ts
import { getEntityHandler } from "@/lib/backend/BaseEntityServer";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import "@/lib/backend/registerAllEntities";
import { NextRequest, NextResponse } from "next/server";

export async function GET(
    request: NextRequest,
    { params }: { params: Promise<{ entity: string }> }
) {
    const resolvedParams = await params;
    try {
        const db = await PostgreSQLDatabaseService.getDataSource();
        const { entity } = resolvedParams;
        const handler = getEntityHandler(entity);
        const repo = db.getRepository(entity);
        //---------------------------------
        const { searchParams } = new URL(request.url);
        // Extract filter
        const filter: Record<string, any> = {};
        searchParams.forEach((value, key) => {
            if (key.startsWith("filter[")) {
                const field = key.slice(7, -1);
                filter[field] = value;
            }
        });
        //---------------------------------
        // Sorting
        const sortField = searchParams.get("sort") ?? "lastLogin";
        const sortOrder = (searchParams.get("order") ?? "desc").toUpperCase();
        //---------------------------------
        // Pagination
        const skip = parseInt(searchParams.get("skip") ?? "0", 10);
        const limit = parseInt(searchParams.get("limit") ?? "100", 10);
        //---------------------------------
        const qb = repo.createQueryBuilder("e");
        //---------------------------------
        Object.entries(filter).forEach(([key, value]) => {
            qb.andWhere(`e.${key} = :${key}`, { [key]: value });
        });
        qb.orderBy(`e.${sortField}`, sortOrder === "ASC" ? "ASC" : "DESC");
        qb.skip(skip).take(limit);
        //---------------------------------
        const results = await qb.getMany();
        return NextResponse.json(results.map(handler.toDTO));
    } catch (error) {
        console.error(
            `Error in GET /api/entities/${resolvedParams.entity}:`,
            error
        );
        return NextResponse.json(
            {
                error: "Internal Server Error",
                message:
                    error instanceof Error ? error.message : "Unknown error",
            },
            { status: 500 }
        );
    }
}

export async function POST(
    request: NextRequest,
    { params }: { params: Promise<{ entity: string }> }
) {
    const resolvedParams = await params;
    try {
        const db = await PostgreSQLDatabaseService.getDataSource();
        const { entity } = resolvedParams;
        const handler = getEntityHandler(entity);
        const repo = db.getRepository(entity);
        //---------------------------------
        const body = await request.json();
        const instance = handler.fromDTO(body);
        const saved = await repo.save(instance);
        return NextResponse.json(handler.toDTO(saved));
    } catch (error) {
        console.error(
            `Error in POST /api/entities/${resolvedParams.entity}:`,
            error
        );
        return NextResponse.json(
            {
                error: "Internal Server Error",
                message:
                    error instanceof Error ? error.message : "Unknown error",
            },
            { status: 500 }
        );
    }
}
