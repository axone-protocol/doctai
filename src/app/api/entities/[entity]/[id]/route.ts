// app/api/entities/[entity]/[id]/route.ts
import { getEntityHandler } from "@/lib/backend/BaseEntityServer";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import "@/lib/backend/registerAllEntities";
import { NextRequest, NextResponse } from "next/server";

export async function GET(
    request: NextRequest,
    { params }: { params: Promise<{ entity: string; id: string }> }
) {
    const resolvedParams = await params;
    try {
        const db = await PostgreSQLDatabaseService.getDataSource();
        const { entity, id } = resolvedParams;
        const handler = getEntityHandler(entity);
        const repo = db.getRepository(entity);
        //---------------------------------
        const one = await repo.findOneBy({ id });
        if (!one) {
            return NextResponse.json({ error: "Not found" }, { status: 404 });
        }
        return NextResponse.json(handler.toDTO(one));
    } catch (error) {
        console.error(
            `Error in GET /api/entities/${resolvedParams.entity}/${resolvedParams.id}:`,
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

export async function PUT(
    request: NextRequest,
    { params }: { params: Promise<{ entity: string; id: string }> }
) {
    const resolvedParams = await params;
    try {
        const db = await PostgreSQLDatabaseService.getDataSource();
        const { entity, id } = resolvedParams;
        const handler = getEntityHandler(entity);
        const repo = db.getRepository(entity);
        //---------------------------------
        const body = await request.json();
        let entityDB = await repo.findOneBy({ id });
        if (!entityDB) {
            return NextResponse.json({ error: "Not found" }, { status: 404 });
        }
        const update = handler.fromDTO({ ...handler.toDTO(entityDB), ...body });
        const saved = await repo.save(update);
        return NextResponse.json(handler.toDTO(saved));
    } catch (error) {
        console.error(
            `Error in PUT /api/entities/${resolvedParams.entity}/${resolvedParams.id}:`,
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

export async function DELETE(
    request: NextRequest,
    { params }: { params: Promise<{ entity: string; id: string }> }
) {
    const resolvedParams = await params;
    try {
        const db = await PostgreSQLDatabaseService.getDataSource();
        const { entity, id } = resolvedParams;
        const repo = db.getRepository(entity);
        await repo.delete({ id });
        return new Response(null, { status: 204 });
    } catch (error) {
        console.error(
            `Error in DELETE /api/entities/${resolvedParams.entity}/${resolvedParams.id}:`,
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
